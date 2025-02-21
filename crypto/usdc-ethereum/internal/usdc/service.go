package usdc

import (
	"fmt"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/models"
	"time"

	"github.com/ethereum/go-ethereum/core/types"
)

// USDCService handles USDC-related operations
type USDCService struct {
	ethClient       ethClientInterface
	etherscanClient *clients.EtherscanClient
}

// NewUSDCService creates a new USDC service instance
func NewUSDCService(ethClient ethClientInterface, etherscanKey string) *USDCService {
	return &USDCService{
		ethClient:       ethClient,
		etherscanClient: clients.NewEtherscanClient(etherscanKey),
	}
}

// FetchUSDCContractEventsByDateRange fetches USDC transfer events within the specified date range
func (s *USDCService) FetchUSDCContractEventsByDateRange(startDate, endDate time.Time, contractAddress string) ([]models.EventLog, error) {
	fromBlock, toBlock, err := getBlockRange(s.ethClient, startDate, endDate)
	if err != nil {
		return nil, err
	}

	fmt.Printf("Fetching USDC history from block %d to block %d\n", fromBlock, toBlock)

	var allEvents []models.EventLog
	currentFromBlock := fromBlock

	// Initialize the block header cache
	blockHeaderCache := make(map[uint64]*types.Header)

	requests := 0 // Added to avoid being rate limited
	for currentFromBlock <= toBlock && requests < requestsLimit {
		currentToBlock := calculateToBlock(currentFromBlock, toBlock, batchSize)

		logs, err := fetchLogs(s.ethClient, currentFromBlock, currentToBlock)
		if err != nil {
			return nil, err
		}

		fmt.Printf("Found %d logs in this batch\n", len(logs))

		events := processLogs(logs, s.ethClient, blockHeaderCache)
		allEvents = append(allEvents, events...)

		currentFromBlock = currentToBlock + 1
		requests++
	}

	return allEvents, nil
}

// FetchLastTransactionsFromWallet fetches USDC transactions for a wallet from Etherscan
func (s *USDCService) FetchLastTransactionsFromWallet(walletAddress string, numTransactions int) ([]models.EventLog, error) {
	transactions, err := s.etherscanClient.FetchTokenTransactions(walletAddress)
	if err != nil {
		return nil, err
	}

	var events []models.EventLog
	for i, tx := range transactions {
		if i >= numTransactions {
			break
		}

		event, err := createEventFromEtherscanTx(tx)
		if err != nil {
			return nil, err
		}
		events = append(events, event)
	}

	return events, nil
}
