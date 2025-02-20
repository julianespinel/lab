package main

import (
	"fmt"
	"log"

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

	// Fetch USDC events
	fmt.Println("Fetching USDC events between:", cfg.StartDate, "and", cfg.EndDate)
	events, err := ethereum.FetchUSDCContractLogsByDateRange(client, cfg.StartDate, cfg.EndDate)
	if err != nil {
		log.Fatalf("Error fetching USDC events: %v", err)
	}

	if len(events) == 0 {
		log.Fatalf("No USDC events found")
	}

	ethereum.DisplayEvents(events)
	ethereum.CalculateTotalAmount(events)
	ethereum.CalculateTotalAmountPerAddress(events)
}
