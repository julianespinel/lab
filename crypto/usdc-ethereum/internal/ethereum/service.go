package ethereum

import (
	"fmt"
	"time"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/blocks"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/logs"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"

	"github.com/ethereum/go-ethereum/core/types"
)

// TODO: move batchSize and requestsLimit to eth_client.go
// Avoiding rate limits is responsibility of the client, not the service.

// Maximum number of results per query to avoid rate limiting
const batchSize = 3

// Maximum number of requests to avoid rate limiting
const requestsLimit = 2

// EthereumService handles USDC-related operations
type EthereumService struct {
	ethClient       clients.EthClientInterface
	etherscanClient clients.EtherscanClientInterface
	logService      logs.LogServiceInterface
	blockService    blocks.BlockServiceInterface
}

// NewEthereumService creates a new Ethereum service instance
func NewEthereumService(
	ethClient clients.EthClientInterface,
	etherscanKey string,
	logService logs.LogServiceInterface,
	blockService blocks.BlockServiceInterface,
) *EthereumService {
	return &EthereumService{
		ethClient:       ethClient,
		etherscanClient: clients.NewEtherscanClient(etherscanKey),
		logService:      logService,
		blockService:    blockService,
	}
}

// FetchUSDCContractEventsByDateRange fetches USDC transfer events within the specified date range
func (s *EthereumService) FetchUSDCContractEventsByDateRange(startDate, endDate time.Time, contractAddress string) ([]models.EventLog, error) {
	fromBlock, toBlock, err := s.blockService.GetBlockRange(s.ethClient, startDate, endDate)
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
		currentToBlock := s.blockService.CalculateToBlock(currentFromBlock, toBlock, batchSize)

		logs, err := s.logService.FetchLogs(s.ethClient, currentFromBlock, currentToBlock, contractAddress)
		if err != nil {
			return nil, err
		}

		fmt.Printf("Found %d logs in this batch\n", len(logs))

		events := s.logService.ProcessLogs(logs, s.ethClient, blockHeaderCache)
		allEvents = append(allEvents, events...)

		currentFromBlock = currentToBlock + 1
		requests++
	}

	return allEvents, nil
}

// FetchLastTransactionsFromWallet fetches USDC transactions for a wallet from Etherscan
func (s *EthereumService) FetchLastTransactionsFromWallet(walletAddress string, numTransactions int) ([]models.EventLog, error) {
	transactions, err := s.etherscanClient.FetchTokenTransactions(walletAddress)
	if err != nil {
		return nil, err
	}

	var events []models.EventLog
	for i, tx := range transactions {
		if i >= numTransactions {
			break
		}

		event, err := s.logService.CreateEventLogFromEtherscanTx(tx)
		if err != nil {
			return nil, err
		}
		events = append(events, event)
	}

	return events, nil
}
