package ethereum

import (
	"context"
	"fmt"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"math/big"
	"time"
)

func getBlockRange(client clients.EthClientInterface, startDate, endDate time.Time) (uint64, uint64, error) {
	fromBlock, err := dateToBlock(client, startDate)
	if err != nil {
		return 0, 0, fmt.Errorf("error converting start date to block: %w", err)
	}

	toBlock, err := dateToBlock(client, endDate)
	if err != nil {
		return 0, 0, fmt.Errorf("error converting end date to block: %w", err)
	}

	return fromBlock, toBlock, nil
}

func calculateToBlock(currentFromBlock, toBlock, batchSize uint64) uint64 {
	currentToBlock := currentFromBlock + batchSize
	if currentToBlock > toBlock {
		currentToBlock = toBlock
	}
	return currentToBlock
}

// dateToBlock converts a time.Time to the nearest block number
// Note: This is an approximation based on average block time (~12 seconds).
// For exact block numbers, we would need to perform a binary search over block timestamps,
// but we avoid this to prevent rate limiting from the Ethereum API provider.
func dateToBlock(client clients.EthClientInterface, date time.Time) (uint64, error) {
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

	blockTime := time.Unix(int64(header.Time), 0)
	// If the requested date is in the future, return the latest block
	if date.After(blockTime) {
		return latestBlock, nil
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
