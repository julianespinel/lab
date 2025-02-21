package blocks

import (
	"time"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
)

type BlockServiceInterface interface {
	GetBlockRange(client clients.EthClientInterface, startDate, endDate time.Time) (uint64, uint64, error)
	CalculateToBlock(currentFromBlock, toBlock, batchSize uint64) uint64
}
