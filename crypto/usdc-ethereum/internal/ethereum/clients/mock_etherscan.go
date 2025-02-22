package clients

import (
	"github.com/stretchr/testify/mock"
)

// MockEtherscanClient is a mock implementation of EtherscanClient
type MockEtherscanClient struct {
	mock.Mock
}

func (m *MockEtherscanClient) FetchTokenTransactions(walletAddress string) ([]EtherscanTransaction, error) {
	args := m.Called(walletAddress)
	return args.Get(0).([]EtherscanTransaction), args.Error(1)
}
