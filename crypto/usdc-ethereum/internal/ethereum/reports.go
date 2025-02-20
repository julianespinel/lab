package ethereum

import (
	"fmt"
	"math/big"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/pkg/models"
)

// DisplayEvents prints the details of each event in a human-readable format.
func DisplayEvents(events []models.ContractEvent) {
	for _, event := range events {
		// Convert the amount to a human-readable format
		humanReadableAmount := new(big.Float).Quo(new(big.Float).SetInt(event.Amount), big.NewFloat(1e6))
		fmt.Printf("%s: %s USDC from %s to %s (Block: %d)\n",
			event.Type, humanReadableAmount.Text('f', 6), event.From.Hex(), event.To.Hex(), event.BlockNumber)
	}
}

// CalculateTotalAmount calculates and prints the total amount of USDC transferred.
func CalculateTotalAmount(events []models.ContractEvent) {
	totalAmount := big.NewInt(0)
	for _, event := range events {
		totalAmount.Add(totalAmount, event.Amount)
	}

	fmt.Printf("Total USDC transferred: %s\n", totalAmount.String())
}

// CalculateTotalAmountPerAddress calculates and prints the total amount of USDC transferred per address.
func CalculateTotalAmountPerAddress(events []models.ContractEvent) {
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
