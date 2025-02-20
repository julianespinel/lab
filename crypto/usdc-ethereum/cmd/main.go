package main

import (
	"fmt"
	"log"
	"math/big"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/config"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum"
)

func main() {
	// Load configuration
	cfg, err := config.LoadConfig("config/config.yaml")
	if err != nil {
		log.Fatalf("Error loading config: %v", err)
	}

	// Connect to Ethereum
	client, err := ethereum.NewClient(cfg.InfuraURL)
	if err != nil {
		log.Fatalf("Failed to connect to Ethereum: %v", err)
	}
	defer client.Close()

	// Fetch USDC transactions
	fmt.Println("Fetching USDC transactions for:", cfg.WalletAddress, cfg.StartDate, cfg.EndDate)
	events, err := ethereum.FetchUSDCContractLogsByDateRange(client, cfg.StartDate, cfg.EndDate)
	if err != nil {
		log.Fatalf("Error fetching USDC transactions: %v", err)
	}

	if len(events) == 0 {
		log.Fatalf("No transactions found")
	}

	// Display results
	for _, event := range events {
		// Convert the amount to a human-readable format
		humanReadableAmount := new(big.Float).Quo(new(big.Float).SetInt(event.Amount), big.NewFloat(1e6))
		fmt.Printf("%s: %s USDC from %s to %s (Block: %d)\n",
			event.Type, humanReadableAmount.Text('f', 6), event.From.Hex(), event.To.Hex(), event.BlockNumber)
	}

	// Calculate the total amount of USDC transferred
	totalAmount := big.NewInt(0)
	for _, event := range events {
		totalAmount.Add(totalAmount, event.Amount)
	}

	fmt.Printf("Total USDC transferred: %s\n", totalAmount.String())

	// Calculate the total amount of USDC transferred per address
	totalAmountPerAddress := make(map[string]*big.Int)
	for _, event := range events {
		// Initialize if not exists
		if _, exists := totalAmountPerAddress[event.From.Hex()]; !exists {
			totalAmountPerAddress[event.From.Hex()] = big.NewInt(0)
		}
		if _, exists := totalAmountPerAddress[event.To.Hex()]; !exists {
			totalAmountPerAddress[event.To.Hex()] = big.NewInt(0)
		}

		// Subtract from sender, add to receiver
		totalAmountPerAddress[event.From.Hex()].Sub(totalAmountPerAddress[event.From.Hex()], event.Amount)
		totalAmountPerAddress[event.To.Hex()].Add(totalAmountPerAddress[event.To.Hex()], event.Amount)
	}

	// Display the total amount of USDC transferred per address
	for address, amount := range totalAmountPerAddress {
		fmt.Printf("Total USDC transferred from/to %s: %s\n", address, amount.String())
	}
}
