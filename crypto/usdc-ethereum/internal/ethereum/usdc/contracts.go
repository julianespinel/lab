package usdc

import (
	"context"
	"fmt"
	"math/big"
	"time"

	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/ethclient"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/pkg/models"
)

func FetchUSDCContractLogsByDateRange(client *ethclient.Client, startDate, endDate time.Time) ([]models.ContractEvent, error) {
	fromBlock, toBlock, err := getBlockRange(client, startDate, endDate)
	if err != nil {
		return nil, err
	}

	fmt.Printf("Fetching USDC history from block %d to block %d\n", fromBlock, toBlock)

	var allEvents []models.ContractEvent
	currentFromBlock := fromBlock

	// Initialize the block header cache
	blockHeaderCache := make(map[uint64]*types.Header)

	requests := 0 // Added to avoid being rate limited
	for currentFromBlock <= toBlock && requests < requestsLimit {
		currentToBlock := calculateToBlock(currentFromBlock, toBlock, batchSize)

		logs, err := fetchLogs(client, currentFromBlock, currentToBlock)
		if err != nil {
			return nil, err
		}

		fmt.Printf("Found %d logs in this batch\n", len(logs))

		events := processLogs(logs, common.HexToAddress(USDCContractAddress), client, blockHeaderCache)
		allEvents = append(allEvents, events...)

		currentFromBlock = currentToBlock + 1
		requests++
	}

	return allEvents, nil
}

func getBlockRange(client *ethclient.Client, startDate, endDate time.Time) (uint64, uint64, error) {
	fromBlock, err := dateToBlock(client, startDate)
	if err != nil {
		return 0, 0, fmt.Errorf("error converting start date to block: %w", err)
	}

	toBlock, err := dateToBlock(client, endDate)
	if err != nil {
		return 0, 0, fmt.Errorf("error converting end date to block: %w", err)
	}

	return fromBlock, toBlock, nil
}

func calculateToBlock(currentFromBlock, toBlock, batchSize uint64) uint64 {
	currentToBlock := currentFromBlock + batchSize
	if currentToBlock > toBlock {
		currentToBlock = toBlock
	}
	return currentToBlock
}

func fetchLogs(client *ethclient.Client, fromBlock, toBlock uint64) ([]types.Log, error) {
	query := ethereum.FilterQuery{
		FromBlock: big.NewInt(int64(fromBlock)),
		ToBlock:   big.NewInt(int64(toBlock)),
		Addresses: []common.Address{common.HexToAddress(USDCContractAddress)},
		Topics:    [][]common.Hash{{common.HexToHash(TransferEventSignature)}},
	}

	logs, err := client.FilterLogs(context.Background(), query)
	if err != nil {
		return nil, fmt.Errorf("error filtering logs for blocks %d-%d: %w", fromBlock, toBlock, err)
	}

	return logs, nil
}

func processLogs(logs []types.Log, usdcContractAddr common.Address, client *ethclient.Client, blockHeaderCache map[uint64]*types.Header) []models.ContractEvent {
	var events []models.ContractEvent

	for _, logEntry := range logs {
		// ERC-20 Transfer events must have 3 topics:
		// Topics[0] = Event signature hash
		// Topics[1] = From address (indexed)
		// Topics[2] = To address (indexed)
		if len(logEntry.Topics) < 3 {
			continue
		}

		from := common.HexToAddress(logEntry.Topics[1].Hex())
		to := common.HexToAddress(logEntry.Topics[2].Hex())
		amount := new(big.Int).SetBytes(logEntry.Data)

		// Check if the block header is already cached
		header, exists := blockHeaderCache[logEntry.BlockNumber]
		if !exists {
			fmt.Printf("Cache miss for block number: %d\n", logEntry.BlockNumber)
			var err error
			header, err = client.HeaderByNumber(context.Background(), big.NewInt(int64(logEntry.BlockNumber)))
			if err != nil {
				fmt.Printf("Error getting block header: %v\n", err)
				continue
			}
			// Cache the block header
			blockHeaderCache[logEntry.BlockNumber] = header
		}

		event := models.ContractEvent{
			Type:        "TRANSFER",
			From:        from,
			To:          to,
			Amount:      amount,
			BlockNumber: logEntry.BlockNumber,
			Timestamp:   time.Unix(int64(header.Time), 0),
		}

		events = append(events, event)
	}

	return events
}
