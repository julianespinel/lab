package ethereum

import (
	"fmt"
	"testing"
	"time"

	"github.com/ethereum/go-ethereum/core/types"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/blocks"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/logs"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestFetchUSDCContractEventsByDateRange_Success_ReturnsEvents(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	startDate := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2024, 1, 2, 0, 0, 0, 0, time.UTC)
	contractAddress := "0xcontract"

	// Mock GetBlockRange
	mockBlockService.On("GetBlockRange", mockEthClient, startDate, endDate).
		Return(uint64(1000), uint64(1002), nil)

	// Mock CalculateToBlock
	mockBlockService.On("CalculateToBlock", uint64(1000), uint64(1002), uint64(batchSize)).
		Return(uint64(1002))

	// Mock FetchLogs
	mockLogs := []types.Log{{BlockNumber: 1000}, {BlockNumber: 1001}}
	mockLogService.On("FetchLogs", mockEthClient, uint64(1000), uint64(1002), contractAddress).
		Return(mockLogs, nil)

	// Mock ProcessLogs
	expectedEvents := []models.EventLog{{BlockNumber: 1000}, {BlockNumber: 1001}}
	mockLogService.On("ProcessLogs", mockLogs, mockEthClient, mock.Anything).
		Return(expectedEvents)

	// Act
	events, err := service.FetchUSDCContractEventsByDateRange(startDate, endDate, contractAddress)

	// Assert
	assert.NoError(t, err)
	assert.Equal(t, expectedEvents, events)
	mockBlockService.AssertExpectations(t)
	mockLogService.AssertExpectations(t)
}

func TestFetchUSDCContractEventsByDateRange_GetBlockRangeError_ReturnsError(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	startDate := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2024, 1, 2, 0, 0, 0, 0, time.UTC)
	contractAddress := "0xcontract"

	expectedError := fmt.Errorf("block range error")
	mockBlockService.On("GetBlockRange", mockEthClient, startDate, endDate).
		Return(uint64(0), uint64(0), expectedError)

	// Act
	events, err := service.FetchUSDCContractEventsByDateRange(startDate, endDate, contractAddress)

	// Assert
	assert.Error(t, err)
	assert.Equal(t, expectedError, err)
	assert.Nil(t, events)
	mockBlockService.AssertExpectations(t)
}

func TestFetchUSDCContractEventsByDateRange_FetchLogsError_ReturnsError(t *testing.T) {
	// Arrange
	mockEthClient := &clients.MockEthClient{}
	mockBlockService := &blocks.MockBlockService{}
	mockLogService := &logs.MockLogService{}

	service := NewUSDCService(mockEthClient, "dummy-key", mockLogService, mockBlockService)

	startDate := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2024, 1, 2, 0, 0, 0, 0, time.UTC)
	contractAddress := "0xcontract"

	mockBlockService.On("GetBlockRange", mockEthClient, startDate, endDate).
		Return(uint64(1000), uint64(1002), nil)

	mockBlockService.On("CalculateToBlock", uint64(1000), uint64(1002), uint64(batchSize)).
		Return(uint64(1002))

	expectedError := fmt.Errorf("fetch logs error")
	mockLogService.On("FetchLogs", mockEthClient, uint64(1000), uint64(1002), contractAddress).
		Return([]types.Log{}, expectedError)

	// Act
	events, err := service.FetchUSDCContractEventsByDateRange(startDate, endDate, contractAddress)

	// Assert
	assert.Error(t, err)
	assert.Equal(t, expectedError, err)
	assert.Nil(t, events)
	mockBlockService.AssertExpectations(t)
	mockLogService.AssertExpectations(t)
}
