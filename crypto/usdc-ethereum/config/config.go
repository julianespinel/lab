package config

import (
	"fmt"
	"os"
	"time"

	"gopkg.in/yaml.v3"
)

type Config struct {
	InfuraURL       string    `yaml:"infura_url"`
	WalletAddress   string    `yaml:"wallet_address"`
	EtherscanAPIKey string    `yaml:"etherscan_api_key"`
	StartDate       time.Time `yaml:"-"`
	EndDate         time.Time `yaml:"-"`
	StartDateStr    string    `yaml:"start_date"`
	EndDateStr      string    `yaml:"end_date"`
}

func LoadConfig(filePath string) (*Config, error) {
	file, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to read config file: %v", err)
	}

	var cfg Config
	if err := yaml.Unmarshal(file, &cfg); err != nil {
		return nil, fmt.Errorf("failed to parse config: %v", err)
	}

	// Parse start date
	startDate, err := time.Parse("2006-01-02", cfg.StartDateStr)
	if err != nil {
		return nil, fmt.Errorf("invalid start_date format: %v", err)
	}
	cfg.StartDate = startDate.UTC()

	// Parse end date
	endDate, err := time.Parse("2006-01-02", cfg.EndDateStr)
	if err != nil {
		return nil, fmt.Errorf("invalid end_date format: %v", err)
	}
	cfg.EndDate = endDate.UTC()

	return &cfg, nil
}
