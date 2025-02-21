package usdc

import (
	"testing"
	"time"

	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestGetBlockRange_ValidDateRange_ReturnsBlockRange(t *testing.T) {
	mockClient := new(MockEthClient)

	// Mock data
	endBlockNumber := uint64(1001000)
	blockTime := time.Now().Unix()

	// Setup mock responses
	mockClient.On("BlockNumber", mock.Anything).Return(endBlockNumber, nil)
	mockClient.On("HeaderByNumber", mock.Anything, mock.Anything).Return(&types.Header{
		Time: uint64(blockTime),
	}, nil)

	// Test dates
	startDate := time.Unix(blockTime, 0).Add(-2 * time.Hour)
	endDate := time.Unix(blockTime, 0).Add(-1 * time.Hour)

	fromBlock, toBlock, err := getBlockRange(mockClient, startDate, endDate)

	assert.NoError(t, err)
	assert.Greater(t, fromBlock, uint64(0))
	assert.Greater(t, toBlock, fromBlock)
}

func TestGetBlockRange_StartDateError_ReturnsError(t *testing.T) {
	mockClient := new(MockEthClient)

	// Mock error for start date conversion
	mockClient.On("BlockNumber", mock.Anything).Return(uint64(0), ethereum.NotFound)

	startDate := time.Now().Add(-1 * time.Hour)
	endDate := time.Now()

	_, _, err := getBlockRange(mockClient, startDate, endDate)

	assert.Error(t, err)
	assert.Contains(t, err.Error(), "error converting start date to block")
}

func TestGetBlockRange_EndDateError_ReturnsError(t *testing.T) {
	mockClient := new(MockEthClient)

	// First call succeeds (for start date)
	mockClient.On("BlockNumber", mock.Anything).Return(uint64(1000000), nil).Once()
	mockClient.On("HeaderByNumber", mock.Anything, mock.Anything).Return(&types.Header{
		Time: uint64(time.Now().Unix()),
	}, nil).Once()

	// Second call fails (for end date)
	mockClient.On("BlockNumber", mock.Anything).Return(uint64(0), ethereum.NotFound).Once()

	startDate := time.Now().Add(-1 * time.Hour)
	endDate := time.Now()

	_, _, err := getBlockRange(mockClient, startDate, endDate)

	assert.Error(t, err)
	assert.Contains(t, err.Error(), "error converting end date to block")
}

func TestCalculateToBlock_WithinBatchSize_ReturnsNextBatch(t *testing.T) {
	currentFromBlock := uint64(1000)
	toBlock := uint64(2000)
	batchSize := uint64(100)

	result := calculateToBlock(currentFromBlock, toBlock, batchSize)

	assert.Equal(t, uint64(1100), result)
}

func TestCalculateToBlock_ExceedingToBlock_ReturnsToBlock(t *testing.T) {
	currentFromBlock := uint64(1900)
	toBlock := uint64(2000)
	batchSize := uint64(200)

	result := calculateToBlock(currentFromBlock, toBlock, batchSize)

	assert.Equal(t, toBlock, result)
}

func TestCalculateToBlock_EqualBlocks_ReturnsToBlock(t *testing.T) {
	currentFromBlock := uint64(2000)
	toBlock := uint64(2000)
	batchSize := uint64(100)

	result := calculateToBlock(currentFromBlock, toBlock, batchSize)

	assert.Equal(t, toBlock, result)
}

func TestFetchLogs_Success_ReturnsLogs(t *testing.T) {
	mockClient := new(MockEthClient)

	expectedLogs := []types.Log{
		{
			BlockNumber: 1000,
			TxHash:      common.HexToHash("0x123"),
		},
	}

	mockClient.On("FilterLogs", mock.Anything, mock.MatchedBy(func(query ethereum.FilterQuery) bool {
		return query.FromBlock.Uint64() == 1000 && query.ToBlock.Uint64() == 2000
	})).Return(expectedLogs, nil)

	logs, err := fetchLogs(mockClient, 1000, 2000)

	assert.NoError(t, err)
	assert.Equal(t, expectedLogs, logs)
}

func TestFetchLogs_FilterError_ReturnsError(t *testing.T) {
	mockClient := new(MockEthClient)

	mockClient.On("FilterLogs", mock.Anything, mock.Anything).Return([]types.Log{}, ethereum.NotFound)

	logs, err := fetchLogs(mockClient, 1000, 2000)

	assert.Error(t, err)
	assert.Empty(t, logs)
	assert.Contains(t, err.Error(), "error filtering logs for blocks")
}
