package ethereum

import (
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"math/big"
	"testing"
	"time"

	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestFetchLogs_Success_ReturnsLogs(t *testing.T) {
	mockClient := new(clients.MockEthClient)

	expectedLogs := []types.Log{
		{
			BlockNumber: 1000,
			TxHash:      common.HexToHash("0x123"),
		},
	}

	mockClient.On("FilterLogs", mock.Anything, mock.MatchedBy(func(query ethereum.FilterQuery) bool {
		return query.FromBlock.Uint64() == 1000 && query.ToBlock.Uint64() == 2000
	})).Return(expectedLogs, nil)

	logs, err := fetchLogs(mockClient, 1000, 2000, USDCContractAddress)

	assert.NoError(t, err)
	assert.Equal(t, expectedLogs, logs)
}

func TestFetchLogs_FilterError_ReturnsError(t *testing.T) {
	mockClient := new(clients.MockEthClient)

	mockClient.On("FilterLogs", mock.Anything, mock.Anything).Return([]types.Log{}, ethereum.NotFound)

	logs, err := fetchLogs(mockClient, 1000, 2000, USDCContractAddress)

	assert.Error(t, err)
	assert.Empty(t, logs)
	assert.Contains(t, err.Error(), "error filtering logs for blocks")
}

func TestProcessLogs_ValidTransferLogs_ReturnsEvents(t *testing.T) {
	mockClient := new(clients.MockEthClient)
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
	events := processLogs(logs, mockClient, blockHeaderCache)

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

	logs := []types.Log{
		{
			BlockNumber: 1000,
			Topics:      []common.Hash{common.HexToHash(transferEventSignature)}, // Missing from/to addresses
		},
	}

	blockHeaderCache := make(map[uint64]*types.Header)
	events := processLogs(logs, mockClient, blockHeaderCache)

	assert.Empty(t, events)
}

func TestProcessLogs_HeaderError_SkipsLog(t *testing.T) {
	mockClient := new(clients.MockEthClient)

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
	events := processLogs(logs, mockClient, blockHeaderCache)

	assert.Empty(t, events)
}
