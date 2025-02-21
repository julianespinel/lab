package logs

import (
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
)

// USDCContractAddress USDC contract address
const USDCContractAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eb48"

// Transfer event signature (Keccak256 of Transfer(address,address,uint256))
const transferEventSignature = "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"

type LogServiceInterface interface {
	FetchLogs(client clients.EthClientInterface, fromBlock, toBlock uint64, contractAddress string) ([]types.Log, error)
	ProcessLogs(logs []types.Log, client clients.EthClientInterface, blockHeaderCache map[uint64]*types.Header) []models.EventLog
	CreateEventLogFromEtherscanTx(tx clients.EtherscanTransaction) (models.EventLog, error)
}
