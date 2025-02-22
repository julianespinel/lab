package blocks

import (
	"time"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/stretchr/testify/mock"
)

// MockBlockService is a mock implementation of BlockServiceInterface
type MockBlockService struct {
	mock.Mock
}

func (m *MockBlockService) GetBlockRange(client clients.EthClientInterface, startDate, endDate time.Time) (uint64, uint64, error) {
	args := m.Called(client, startDate, endDate)
	return args.Get(0).(uint64), args.Get(1).(uint64), args.Error(2)
}

func (m *MockBlockService) CalculateToBlock(fromBlock, toBlock uint64, batchSize uint64) uint64 {
	args := m.Called(fromBlock, toBlock, batchSize)
	return args.Get(0).(uint64)
}
