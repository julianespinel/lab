package ethereum

import (
	"context"
	"fmt"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
	"math/big"
	"strconv"
	"time"

	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
)

// USDCContractAddress USDC contract address
const USDCContractAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eb48"

// Transfer event signature (Keccak256 of Transfer(address,address,uint256))
const transferEventSignature = "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"

func fetchLogs(client clients.EthClientInterface, fromBlock, toBlock uint64, contractAddress string) ([]types.Log, error) {
	query := ethereum.FilterQuery{
		FromBlock: big.NewInt(int64(fromBlock)),
		ToBlock:   big.NewInt(int64(toBlock)),
		Addresses: []common.Address{common.HexToAddress(contractAddress)},
		Topics:    [][]common.Hash{{common.HexToHash(transferEventSignature)}},
	}

	logs, err := client.FilterLogs(context.Background(), query)
	if err != nil {
		return nil, fmt.Errorf("error filtering logs for blocks %d-%d: %w", fromBlock, toBlock, err)
	}

	return logs, nil
}

func processLogs(logs []types.Log, client clients.EthClientInterface, blockHeaderCache map[uint64]*types.Header) []models.EventLog {
	var events []models.EventLog

	for _, logEntry := range logs {
		// ERC-20 Transfer events must have 3 topics:
		// Topics[0] = EventLog signature hash
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

		event := models.EventLog{
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

// createEventLogFromEtherscanTx converts an Etherscan transaction to a EventLog
func createEventLogFromEtherscanTx(tx clients.EtherscanTransaction) (models.EventLog, error) {
	blockNumber, err := strconv.ParseUint(tx.BlockNumber, 10, 64)
	if err != nil {
		return models.EventLog{}, fmt.Errorf("error parsing block number: %v", err)
	}

	timeStamp, err := strconv.ParseInt(tx.TimeStamp, 10, 64)
	if err != nil {
		return models.EventLog{}, fmt.Errorf("error parsing timestamp: %v", err)
	}

	amount, success := new(big.Int).SetString(tx.Value, 10)
	if !success {
		return models.EventLog{}, fmt.Errorf("error parsing transaction value: %s", tx.Value)
	}

	return models.EventLog{
		Type:        "TOKEN_TRANSFER",
		From:        common.HexToAddress(tx.From),
		To:          common.HexToAddress(tx.To),
		Amount:      amount,
		BlockNumber: blockNumber,
		Timestamp:   time.Unix(timeStamp, 0),
	}, nil
}
