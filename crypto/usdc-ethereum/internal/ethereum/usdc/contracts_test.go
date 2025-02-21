package usdc

import (
	"math/big"
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

func TestProcessLogs_ValidTransferLogs_ReturnsEvents(t *testing.T) {
	mockClient := new(MockEthClient)
	blockTime := uint64(time.Now().Unix())

	// Mock the block header response
	mockClient.On("HeaderByNumber", mock.Anything, big.NewInt(1000)).Return(&types.Header{
		Time: blockTime,
	}, nil)

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics: []common.Hash{
				common.HexToHash(TransferEventSignature),
				// Ethereum addresses (20 bytes) must be padded to 32 bytes when used as event topics
				// "0x" + "000000000000000000000000" (12 bytes of padding) + "1111..." (20 byte address)
				common.HexToHash("0x000000000000000000000000" + "1111111111111111111111111111111111111111"),
				common.HexToHash("0x000000000000000000000000" + "2222222222222222222222222222222222222222"),
			},
			Data: new(big.Int).SetUint64(1000000).Bytes(), // Amount
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := processLogs(logs, common.HexToAddress(USDCContractAddress), mockClient, blockHeaderCache)

	assert.Len(t, events, 1)
	assert.Equal(t, "TRANSFER", events[0].Type)
	assert.Equal(t, "0x1111111111111111111111111111111111111111", events[0].From.Hex())
	assert.Equal(t, "0x2222222222222222222222222222222222222222", events[0].To.Hex())
	assert.Equal(t, big.NewInt(1000000), events[0].Amount)
	assert.Equal(t, uint64(1000), events[0].BlockNumber)
	assert.Equal(t, time.Unix(int64(blockTime), 0), events[0].Timestamp)
}

func TestProcessLogs_InvalidTopics_SkipsLog(t *testing.T) {
	mockClient := new(MockEthClient)

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics:      []common.Hash{common.HexToHash(TransferEventSignature)}, // Missing from/to addresses
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := processLogs(logs, common.HexToAddress(USDCContractAddress), mockClient, blockHeaderCache)

	assert.Empty(t, events)
}

func TestProcessLogs_HeaderError_SkipsLog(t *testing.T) {
	mockClient := new(MockEthClient)

	mockClient.On("HeaderByNumber", mock.Anything, big.NewInt(1000)).Return(
		(*types.Header)(nil), ethereum.NotFound,
	)

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics: []common.Hash{
				common.HexToHash(TransferEventSignature),
				common.HexToHash("0x1111111111111111111111111111111111111111"),
				common.HexToHash("0x2222222222222222222222222222222222222222"),
			},
			Data: new(big.Int).SetUint64(1000000).Bytes(),
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := processLogs(logs, common.HexToAddress(USDCContractAddress), mockClient, blockHeaderCache)

	assert.Empty(t, events)
}
