package logs

import (
	"math/big"
	"testing"
	"time"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"

	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestFetchLogs_Success_ReturnsLogs(t *testing.T) {
	mockClient := new(clients.MockEthClient)
	logService := NewLogService()

	expectedLogs := []types.Log{
		{
			BlockNumber: 1000,
			TxHash:      common.HexToHash("0x123"),
		},
	}

	mockClient.On("FilterLogs", mock.Anything, mock.MatchedBy(func(query ethereum.FilterQuery) bool {
		return query.FromBlock.Uint64() == 1000 && query.ToBlock.Uint64() == 2000
	})).Return(expectedLogs, nil)

	logs, err := logService.FetchLogs(mockClient, 1000, 2000, USDCContractAddress)

	assert.NoError(t, err)
	assert.Equal(t, expectedLogs, logs)
}

func TestFetchLogs_FilterError_ReturnsError(t *testing.T) {
	mockClient := new(clients.MockEthClient)
	logService := NewLogService()

	mockClient.On("FilterLogs", mock.Anything, mock.Anything).Return([]types.Log{}, ethereum.NotFound)

	logs, err := logService.FetchLogs(mockClient, 1000, 2000, USDCContractAddress)

	assert.Error(t, err)
	assert.Empty(t, logs)
	assert.Contains(t, err.Error(), "error filtering logs for blocks")
}

func TestProcessLogs_ValidTransferLogs_ReturnsEvents(t *testing.T) {
	mockClient := new(clients.MockEthClient)
	logService := NewLogService()
	blockTime := uint64(time.Now().Unix())

	// Mock the block header response
	mockClient.On("HeaderByNumber", mock.Anything, big.NewInt(1000)).Return(&types.Header{
		Time: blockTime,
	}, nil)

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics: []common.Hash{
				common.HexToHash(transferEventSignature),
				// Ethereum addresses (20 bytes) must be padded to 32 bytes when used as event topics
				// "0x" + "000000000000000000000000" (12 bytes of padding) + "1111..." (20 byte address)
				common.HexToHash("0x000000000000000000000000" + "1111111111111111111111111111111111111111"),
				common.HexToHash("0x000000000000000000000000" + "2222222222222222222222222222222222222222"),
			},
			Data: new(big.Int).SetUint64(1000000).Bytes(), // Amount
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := logService.ProcessLogs(logs, mockClient, blockHeaderCache)

	assert.Len(t, events, 1)
	assert.Equal(t, "TRANSFER", events[0].Type)
	assert.Equal(t, "0x1111111111111111111111111111111111111111", events[0].From.Hex())
	assert.Equal(t, "0x2222222222222222222222222222222222222222", events[0].To.Hex())
	assert.Equal(t, big.NewInt(1000000), events[0].Amount)
	assert.Equal(t, uint64(1000), events[0].BlockNumber)
	assert.Equal(t, time.Unix(int64(blockTime), 0), events[0].Timestamp)
}

func TestProcessLogs_InvalidTopics_SkipsLog(t *testing.T) {
	mockClient := new(clients.MockEthClient)
	logService := NewLogService()

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics:      []common.Hash{common.HexToHash(transferEventSignature)}, // Missing from/to addresses
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := logService.ProcessLogs(logs, mockClient, blockHeaderCache)

	assert.Empty(t, events)
}

func TestProcessLogs_HeaderError_SkipsLog(t *testing.T) {
	mockClient := new(clients.MockEthClient)
	logService := NewLogService()

	mockClient.On("HeaderByNumber", mock.Anything, big.NewInt(1000)).Return(
		(*types.Header)(nil), ethereum.NotFound,
	)

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics: []common.Hash{
				common.HexToHash(transferEventSignature),
				common.HexToHash("0x1111111111111111111111111111111111111111"),
				common.HexToHash("0x2222222222222222222222222222222222222222"),
			},
			Data: new(big.Int).SetUint64(1000000).Bytes(),
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := logService.ProcessLogs(logs, mockClient, blockHeaderCache)

	assert.Empty(t, events)
}

func TestCreateEventLogFromEtherscanTx_Success_ReturnsEventLog(t *testing.T) {
	logService := NewLogService()

	tx := clients.EtherscanTransaction{
		BlockNumber: "1000",
		TimeStamp:   "1234567890",
		From:        "0x1111111111111111111111111111111111111111",
		To:          "0x2222222222222222222222222222222222222222",
		Value:       "1000000",
	}

	event, err := logService.CreateEventLogFromEtherscanTx(tx)

	assert.NoError(t, err)
	assert.Equal(t, "TOKEN_TRANSFER", event.Type)
	assert.Equal(t, uint64(1000), event.BlockNumber)
	assert.Equal(t, time.Unix(1234567890, 0), event.Timestamp)
	assert.Equal(t, "0x1111111111111111111111111111111111111111", event.From.Hex())
	assert.Equal(t, "0x2222222222222222222222222222222222222222", event.To.Hex())
	assert.Equal(t, big.NewInt(1000000), event.Amount)
}

func TestCreateEventLogFromEtherscanTx_InvalidBlockNumber_ReturnsError(t *testing.T) {
	logService := NewLogService()

	tx := clients.EtherscanTransaction{
		BlockNumber: "invalid",
		TimeStamp:   "1234567890",
		From:        "0x1111111111111111111111111111111111111111",
		To:          "0x2222222222222222222222222222222222222222",
		Value:       "1000000",
	}

	_, err := logService.CreateEventLogFromEtherscanTx(tx)

	assert.Error(t, err)
	assert.Contains(t, err.Error(), "error parsing block number")
}

func TestCreateEventLogFromEtherscanTx_InvalidTimestamp_ReturnsError(t *testing.T) {
	logService := NewLogService()

	tx := clients.EtherscanTransaction{
		BlockNumber: "1000",
		TimeStamp:   "invalid",
		From:        "0x1111111111111111111111111111111111111111",
		To:          "0x2222222222222222222222222222222222222222",
		Value:       "1000000",
	}

	_, err := logService.CreateEventLogFromEtherscanTx(tx)

	assert.Error(t, err)
	assert.Contains(t, err.Error(), "error parsing timestamp")
}

func TestCreateEventLogFromEtherscanTx_InvalidValue_ReturnsError(t *testing.T) {
	logService := NewLogService()

	tx := clients.EtherscanTransaction{
		BlockNumber: "1000",
		TimeStamp:   "1234567890",
		From:        "0x1111111111111111111111111111111111111111",
		To:          "0x2222222222222222222222222222222222222222",
		Value:       "invalid",
	}

	_, err := logService.CreateEventLogFromEtherscanTx(tx)

	assert.Error(t, err)
	assert.Contains(t, err.Error(), "error parsing transaction value")
}
