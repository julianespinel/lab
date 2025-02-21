package usdc

import (
	"context"
	"math/big"
	"testing"
	"time"

	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

// MockEthClient embeds ethclient.Client and overrides methods for testing
type MockEthClient struct {
	mock.Mock
}

func (m *MockEthClient) BlockNumber(ctx context.Context) (uint64, error) {
	args := m.Called(ctx)
	return args.Get(0).(uint64), args.Error(1)
}

func (m *MockEthClient) HeaderByNumber(ctx context.Context, number *big.Int) (*types.Header, error) {
	args := m.Called(ctx, number)
	return args.Get(0).(*types.Header), args.Error(1)
}

func TestDateToBlock_PastDate_ReturnsCalculatedBlock(t *testing.T) {
	mockClient := new(MockEthClient)

	latestBlockNumber := uint64(1000000)
	latestBlockTime := time.Now().Unix()

	mockClient.On("BlockNumber", mock.Anything).Return(latestBlockNumber, nil)
	mockClient.On("HeaderByNumber", mock.Anything, big.NewInt(int64(latestBlockNumber))).Return(&types.Header{
		Time: uint64(latestBlockTime),
	}, nil)

	date := time.Unix(latestBlockTime, 0).Add(-1 * time.Hour)
	expectedBlockNumber := latestBlockNumber - uint64(3600/12)

	blockNumber, err := dateToBlock(mockClient, date)
	assert.NoError(t, err)
	assert.Equal(t, expectedBlockNumber, blockNumber)
}

func TestDateToBlock_FutureDate_ReturnsLatestBlock(t *testing.T) {
	mockClient := new(MockEthClient)

	latestBlockNumber := uint64(1000000)
	latestBlockTime := time.Now().Unix()

	mockClient.On("BlockNumber", mock.Anything).Return(latestBlockNumber, nil)
	mockClient.On("HeaderByNumber", mock.Anything, big.NewInt(int64(latestBlockNumber))).Return(&types.Header{
		Time: uint64(latestBlockTime),
	}, nil)

	futureDate := time.Unix(latestBlockTime, 0).Add(1 * time.Hour)
	blockNumber, err := dateToBlock(mockClient, futureDate)
	assert.NoError(t, err)
	assert.Equal(t, latestBlockNumber, blockNumber)
}

func TestDateToBlock_BlockNumberError_ReturnsError(t *testing.T) {
	mockClient := new(MockEthClient)

	mockClient.On("BlockNumber", mock.Anything).Return(uint64(0), ethereum.NotFound)

	date := time.Now()
	_, err := dateToBlock(mockClient, date)
	assert.Error(t, err)
}
