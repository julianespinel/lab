package usdc

import (
	"context"
	"fmt"
	"math/big"
	"time"

	"github.com/ethereum/go-ethereum/core/types"
)

// USDC contract address
const USDCContractAddress = "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eb48"

// Transfer event signature (Keccak256 of Transfer(address,address,uint256))
const TransferEventSignature = "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"

// Maximum number of results per query to avoid rate limiting
const batchSize = 3

// Maximum number of requests to avoid rate limiting
const requestsLimit = 2

// Add this interface to the top of the file
type ethClientInterface interface {
	BlockNumber(ctx context.Context) (uint64, error)
	HeaderByNumber(ctx context.Context, number *big.Int) (*types.Header, error)
}

// dateToBlock converts a time.Time to the nearest block number
// Note: This is an approximation based on average block time (~12 seconds).
// For exact block numbers, we would need to perform a binary search over block timestamps,
// but we avoid this to prevent rate limiting from the Ethereum API provider.
func dateToBlock(client ethClientInterface, date time.Time) (uint64, error) {
	// Get the latest block
	latestBlock, err := client.BlockNumber(context.Background())
	if err != nil {
		return 0, fmt.Errorf("error getting latest block: %w", err)
	}

	// Get the latest block header to get its timestamp
	header, err := client.HeaderByNumber(context.Background(), big.NewInt(int64(latestBlock)))
	if err != nil {
		return 0, fmt.Errorf("error getting latest block header: %w", err)
	}

	// Calculate approximate block number based on target date
	// Ethereum blocks are mined every ~12 seconds on average
	timeDiff := blockTime.Sub(date)
	blockDiff := int64(timeDiff.Seconds() / 12) // 12 seconds per block average

	targetBlock := int64(latestBlock) - blockDiff
	if targetBlock < 0 {
		targetBlock = 0
	}

	return uint64(targetBlock), nil
}
