package clients

import (
	"context"
	"github.com/ethereum/go-ethereum"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/stretchr/testify/mock"
	"math/big"
)

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

func (m *MockEthClient) FilterLogs(ctx context.Context, query ethereum.FilterQuery) ([]types.Log, error) {
	args := m.Called(ctx, query)
	return args.Get(0).([]types.Log), args.Error(1)
}
