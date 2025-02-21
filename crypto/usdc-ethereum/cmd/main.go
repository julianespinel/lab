package main

import (
	"fmt"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
	"log"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/config"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum"
)

func fetchAndProcessContractEvents(usdcService *ethereum.USDCService, cfg *config.Config) ([]models.EventLog, error) {
	fmt.Println("Fetching USDC events between:", cfg.StartDate, "and", cfg.EndDate)
	events, err := usdcService.FetchUSDCContractEventsByDateRange(cfg.StartDate, cfg.EndDate, cfg.USDCContractAddress)
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

func fetchAndProcessWalletTransactions(usdcService *ethereum.USDCService, cfg *config.Config, numTransactions int) ([]models.EventLog, error) {
	fmt.Println("Fetching transactions for wallet:", cfg.WalletAddress)

	events, err := usdcService.FetchLastTransactionsFromWallet(cfg.WalletAddress, numTransactions)
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
	ethClient, err := clients.NewClient(cfg.InfuraURL)
	if err != nil {
		log.Fatalf("Failed to connect to Ethereum: %v", err)
	}
	defer ethClient.Close()

	// Initialize USDC service
	usdcService := ethereum.NewUSDCService(ethClient, cfg.EtherscanAPIKey)

	_, err = fetchAndProcessContractEvents(usdcService, cfg)
	if err != nil {
		log.Fatalf("%v", err)
	}

	_, err = fetchAndProcessWalletTransactions(usdcService, cfg, 10)
	if err != nil {
		log.Fatalf("%v", err)
	}
}
