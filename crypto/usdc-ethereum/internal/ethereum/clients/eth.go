package clients

import (
	"context"
	"fmt"
	"time"

	"github.com/ethereum/go-ethereum/ethclient"
)

// NewClient connects to an Ethereum node.
func NewClient(rpcURL string) (*ethclient.Client, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	client, err := ethclient.Dial(rpcURL)
	if err != nil {
		return nil, fmt.Errorf("failed to connect to Ethereum node at %s: %w", rpcURL, err)
	}

	// Verify connection with a test call
	_, err = client.ChainID(ctx)
	if err != nil {
		return nil, fmt.Errorf("failed to verify connection: %w", err)
	}

	return client, nil
}
