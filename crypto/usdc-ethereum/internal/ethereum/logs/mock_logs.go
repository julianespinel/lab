package logs

import (
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
	"github.com/stretchr/testify/mock"
)

// MockLogService is a mock implementation of LogServiceInterface
type MockLogService struct {
	mock.Mock
}

func (m *MockLogService) FetchLogs(client clients.EthClientInterface, fromBlock, toBlock uint64, contractAddress string) ([]types.Log, error) {
	args := m.Called(client, fromBlock, toBlock, contractAddress)
	return args.Get(0).([]types.Log), args.Error(1)
}

func (m *MockLogService) ProcessLogs(logs []types.Log, client clients.EthClientInterface, blockHeaderCache map[uint64]*types.Header) []models.EventLog {
	args := m.Called(logs, client, blockHeaderCache)
	return args.Get(0).([]models.EventLog)
}

func (m *MockLogService) CreateEventLogFromEtherscanTx(tx clients.EtherscanTransaction) (models.EventLog, error) {
	args := m.Called(tx)
	return args.Get(0).(models.EventLog), args.Error(1)
}
