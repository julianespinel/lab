package main

import (
	"fmt"
	"log"

	"github.com/ethereum/go-ethereum/ethclient"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/config"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/usdc"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/pkg/models"
)

func fetchAndProcessContractEvents(client *ethclient.Client, cfg *config.Config) ([]models.ContractEvent, error) {
	fmt.Println("Fetching USDC events between:", cfg.StartDate, "and", cfg.EndDate)
	events, err := usdc.FetchUSDCContractLogsByDateRange(client, cfg.StartDate, cfg.EndDate)
	if err != nil {
		return nil, fmt.Errorf("error fetching USDC events: %v", err)
	}

	if len(events) == 0 {
		return nil, fmt.Errorf("no USDC events found")
	}

	ethereum.DisplayEvents(events)
	ethereum.CalculateTotalAmount(events)
	ethereum.CalculateTotalAmountPerAddress(events)

	return events, nil
}

func fetchAndProcessWalletTransactions(cfg *config.Config, numTransactions int) ([]models.ContractEvent, error) {
	fmt.Println("Fetching transactions for wallet:", cfg.WalletAddress)

	events, err := usdc.FetchTransactionsFromEtherscan(cfg.EtherscanAPIKey, cfg.WalletAddress, numTransactions)
	if err != nil {
		return nil, fmt.Errorf("error fetching wallet events: %v", err)
	}

	if len(events) == 0 {
		return nil, fmt.Errorf("no wallet events found")
	}

	ethereum.DisplayEvents(events)
	ethereum.CalculateTotalAmount(events)
	ethereum.CalculateTotalAmountPerAddress(events)

	return events, nil
}

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

	_, err = fetchAndProcessContractEvents(client, cfg)
	if err != nil {
		log.Fatalf("%v", err)
	}

	_, err = fetchAndProcessWalletTransactions(cfg, 10)
	if err != nil {
		log.Fatalf("%v", err)
	}
}
