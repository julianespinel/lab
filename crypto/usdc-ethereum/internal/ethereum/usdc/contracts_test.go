package usdc

import (
	"testing"
	"time"

	"github.com/ethereum/go-ethereum"
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
