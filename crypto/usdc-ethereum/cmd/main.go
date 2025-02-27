package main

import (
	"fmt"
	"log"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/blocks"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/logs"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/config"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum"
)

func fetchAndProcessContractEvents(ethService *ethereum.EthereumService, cfg *config.Config) ([]models.EventLog, error) {
	fmt.Println("Fetching events between:", cfg.StartDate, "and", cfg.EndDate, "for contract:", cfg.USDCContractAddress, "(USDC contract)")
	events, err := ethService.FetchUSDCContractEventsByDateRange(cfg.StartDate, cfg.EndDate, cfg.USDCContractAddress)
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

func fetchAndProcessWalletTransactions(ethService *ethereum.EthereumService, cfg *config.Config, numTransactions int) ([]models.EventLog, error) {
	fmt.Println("Fetching transactions for wallet:", cfg.WalletAddress)

	events, err := ethService.FetchLastTransactionsFromWallet(cfg.WalletAddress, numTransactions)
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
	ethService := ethereum.NewEthereumService(
		ethClient,
		cfg.EtherscanAPIKey,
		logs.NewLogService(),
		blocks.NewBlockService(),
	)

	fmt.Println("--------------------------------")
	fmt.Println("Contract events in time range")
	fmt.Println("--------------------------------")

	_, err = fetchAndProcessContractEvents(ethService, cfg)
	if err != nil {
		log.Fatalf("%v", err)
	}

	fmt.Print("\n\n")
	fmt.Println("--------------------------------")
	fmt.Println("Last N wallet transactions")
	fmt.Println("--------------------------------")

	_, err = fetchAndProcessWalletTransactions(ethService, cfg, 8)
	if err != nil {
		log.Fatalf("%v", err)
	}
}
