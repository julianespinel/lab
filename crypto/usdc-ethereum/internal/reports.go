package internal

import (
	"fmt"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/models"
	"math/big"
)

// toHumanReadableAmount converts a big.Int amount to a human-readable big.Float format.
func toHumanReadableAmount(amount *big.Int) *big.Float {
	return new(big.Float).Quo(new(big.Float).SetInt(amount), big.NewFloat(1e6))
}

// DisplayEvents prints the details of each event in a human-readable format.
func DisplayEvents(events []models.Event) {
	for _, event := range events {
		humanReadableAmount := toHumanReadableAmount(event.Amount)
		fmt.Printf("%s: %s USDC from %s to %s (Block: %d)\n",
			event.Type, humanReadableAmount.Text('f', 6), event.From.Hex(), event.To.Hex(), event.BlockNumber)
	}
}

// CalculateTotalAmount calculates and prints the total amount of USDC transferred.
func CalculateTotalAmount(events []models.Event) {
	totalAmount := big.NewInt(0)
	for _, event := range events {
		totalAmount.Add(totalAmount, event.Amount)
	}

	humanReadableTotal := toHumanReadableAmount(totalAmount)
	fmt.Printf("Total USDC transferred: %s\n", humanReadableTotal.Text('f', 6))
}

// CalculateTotalAmountPerAddress calculates and prints the total amount of USDC transferred per address.
func CalculateTotalAmountPerAddress(events []models.Event) {
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

	for address, amount := range totalAmountPerAddress {
		humanReadableAmount := toHumanReadableAmount(amount)
		fmt.Printf("Total USDC transferred from/to %s: %s\n", address, humanReadableAmount.Text('f', 6))
	}
}
