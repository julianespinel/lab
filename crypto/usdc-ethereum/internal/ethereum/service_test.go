package ethereum

import (
	"fmt"
	"math/big"
	"testing"
	"time"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/blocks"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/logs"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestFetchUSDCContractEventsByDateRange_Success_ReturnsEvents(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	startDate := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2024, 1, 2, 0, 0, 0, 0, time.UTC)
	contractAddress := "0xcontract"

	// Mock GetBlockRange
	mockBlockService.On("GetBlockRange", mockEthClient, startDate, endDate).
		Return(uint64(1000), uint64(1002), nil)

	// Mock CalculateToBlock
	mockBlockService.On("CalculateToBlock", uint64(1000), uint64(1002), uint64(batchSize)).
		Return(uint64(1002))

	// Mock FetchLogs
	mockLogs := []types.Log{{BlockNumber: 1000}, {BlockNumber: 1001}}
	mockLogService.On("FetchLogs", mockEthClient, uint64(1000), uint64(1002), contractAddress).
		Return(mockLogs, nil)

	// Mock ProcessLogs
	expectedEvents := []models.EventLog{{BlockNumber: 1000}, {BlockNumber: 1001}}
	mockLogService.On("ProcessLogs", mockLogs, mockEthClient, mock.Anything).
		Return(expectedEvents)

	// Act
	events, err := service.FetchUSDCContractEventsByDateRange(startDate, endDate, contractAddress)

	// Assert
	assert.NoError(t, err)
	assert.Equal(t, expectedEvents, events)
	mockBlockService.AssertExpectations(t)
	mockLogService.AssertExpectations(t)
}

func TestFetchUSDCContractEventsByDateRange_GetBlockRangeError_ReturnsError(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	startDate := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2024, 1, 2, 0, 0, 0, 0, time.UTC)
	contractAddress := "0xcontract"

	expectedError := fmt.Errorf("block range error")
	mockBlockService.On("GetBlockRange", mockEthClient, startDate, endDate).
		Return(uint64(0), uint64(0), expectedError)

	// Act
	events, err := service.FetchUSDCContractEventsByDateRange(startDate, endDate, contractAddress)

	// Assert
	assert.Error(t, err)
	assert.Equal(t, expectedError, err)
	assert.Nil(t, events)
	mockBlockService.AssertExpectations(t)
}

func TestFetchUSDCContractEventsByDateRange_FetchLogsError_ReturnsError(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	startDate := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2024, 1, 2, 0, 0, 0, 0, time.UTC)
	contractAddress := "0xcontract"

	mockBlockService.On("GetBlockRange", mockEthClient, startDate, endDate).
		Return(uint64(1000), uint64(1002), nil)

	mockBlockService.On("CalculateToBlock", uint64(1000), uint64(1002), uint64(batchSize)).
		Return(uint64(1002))

	expectedError := fmt.Errorf("fetch logs error")
	mockLogService.On("FetchLogs", mockEthClient, uint64(1000), uint64(1002), contractAddress).
		Return([]types.Log{}, expectedError)

	// Act
	events, err := service.FetchUSDCContractEventsByDateRange(startDate, endDate, contractAddress)

	// Assert
	assert.Error(t, err)
	assert.Equal(t, expectedError, err)
	assert.Nil(t, events)
	mockBlockService.AssertExpectations(t)
	mockLogService.AssertExpectations(t)
}

func TestFetchLastTransactionsFromWallet_Success_ReturnsEvents(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	walletAddress := "0xwallet"
	numTransactions := 2

	// Mock transactions from Etherscan
	mockTransactions := []clients.EtherscanTransaction{
		{
			BlockNumber: "1000",
			TimeStamp:   "1234567890",
			From:        "0x1",
			To:          "0x2",
			Value:       "100",
		},
		{
			BlockNumber: "1001",
			TimeStamp:   "1234567891",
			From:        "0x3",
			To:          "0x4",
			Value:       "200",
		},
		{
			BlockNumber: "1002",
			TimeStamp:   "1234567892",
			From:        "0x5",
			To:          "0x6",
			Value:       "300",
		}, // This one should be ignored because numTransactions is 2
	}

	// Mock etherscan client response
	service.etherscanClient = &clients.MockEtherscanClient{}
	service.etherscanClient.(*clients.MockEtherscanClient).On("FetchTokenTransactions", walletAddress).
		Return(mockTransactions, nil)

	// Mock event log creation
	expectedEvents := []models.EventLog{
		{
			Type:        "TOKEN_TRANSFER",
			BlockNumber: 1000,
			From:        common.HexToAddress("0x1"),
			To:          common.HexToAddress("0x2"),
			Amount:      big.NewInt(100),
			Timestamp:   time.Unix(1234567890, 0),
		},
		{
			Type:        "TOKEN_TRANSFER",
			BlockNumber: 1001,
			From:        common.HexToAddress("0x3"),
			To:          common.HexToAddress("0x4"),
			Amount:      big.NewInt(200),
			Timestamp:   time.Unix(1234567891, 0),
		},
	}

	mockLogService.On("CreateEventLogFromEtherscanTx", mockTransactions[0]).
		Return(expectedEvents[0], nil)
	mockLogService.On("CreateEventLogFromEtherscanTx", mockTransactions[1]).
		Return(expectedEvents[1], nil)

	// Act
	events, err := service.FetchLastTransactionsFromWallet(walletAddress, numTransactions)

	// Assert
	assert.NoError(t, err)
	assert.Equal(t, expectedEvents, events)
	assert.Len(t, events, numTransactions)
	service.etherscanClient.(*clients.MockEtherscanClient).AssertExpectations(t)
	mockLogService.AssertExpectations(t)
}

func TestFetchLastTransactionsFromWallet_EtherscanError_ReturnsError(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	walletAddress := "0xwallet"
	numTransactions := 2

	// Mock etherscan client error
	expectedError := fmt.Errorf("etherscan api error")
	service.etherscanClient = &clients.MockEtherscanClient{}
	service.etherscanClient.(*clients.MockEtherscanClient).On("FetchTokenTransactions", walletAddress).
		Return([]clients.EtherscanTransaction{}, expectedError)

	// Act
	events, err := service.FetchLastTransactionsFromWallet(walletAddress, numTransactions)

	// Assert
	assert.Error(t, err)
	assert.Equal(t, expectedError, err)
	assert.Nil(t, events)
	service.etherscanClient.(*clients.MockEtherscanClient).AssertExpectations(t)
}

func TestFetchLastTransactionsFromWallet_CreateEventLogError_ReturnsError(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	walletAddress := "0xwallet"
	numTransactions := 2

	// Mock transactions from Etherscan
	mockTransactions := []clients.EtherscanTransaction{
		{
			BlockNumber: "1000",
			TimeStamp:   "1234567890",
			From:        "0x1",
			To:          "0x2",
			Value:       "100",
		},
		{
			BlockNumber: "1001",
			TimeStamp:   "1234567891",
			From:        "0x3",
			To:          "0x4",
			Value:       "200",
		},
	}

	// Mock etherscan client response
	service.etherscanClient = &clients.MockEtherscanClient{}
	service.etherscanClient.(*clients.MockEtherscanClient).On("FetchTokenTransactions", walletAddress).
		Return(mockTransactions, nil)

	// Mock event log creation error
	expectedError := fmt.Errorf("create event log error")
	mockLogService.On("CreateEventLogFromEtherscanTx", mockTransactions[0]).
		Return(models.EventLog{}, expectedError)

	// Act
	events, err := service.FetchLastTransactionsFromWallet(walletAddress, numTransactions)

	// Assert
	assert.Error(t, err)
	assert.Equal(t, expectedError, err)
	assert.Nil(t, events)
	service.etherscanClient.(*clients.MockEtherscanClient).AssertExpectations(t)
	mockLogService.AssertExpectations(t)
}
