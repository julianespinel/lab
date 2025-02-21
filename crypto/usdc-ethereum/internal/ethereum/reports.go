package ethereum

import (
	"fmt"
	"math/big"
	"strings"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
)

// toHumanReadableAmount converts a big.Int amount to a human-readable string format
// with comma separators for thousands.
func toHumanReadableAmount(amount *big.Int) string {
	// Convert the amount from micro USDC to USDC by dividing by 1e6.
	floatAmount := new(big.Float).Quo(new(big.Float).SetInt(amount), big.NewFloat(1e6))

	// Convert the big.Float to a string with 6 decimal places.
	numStr := floatAmount.Text('f', 6)

	// Split into integer and decimal parts.
	parts := strings.Split(numStr, ".")
	integerPart := parts[0]
	decimalPart := parts[1]

	// Add commas to the integer part.
	var result strings.Builder
	for i, char := range integerPart {
		if i > 0 && (len(integerPart)-i)%3 == 0 {
			result.WriteRune(',')
		}
		result.WriteRune(char)
	}

	// Combine the integer and decimal parts.
	return result.String() + "." + decimalPart
}

// DisplayEvents prints the details of each event in a human-readable format.
func DisplayEvents(events []models.EventLog) {
	for _, event := range events {
		humanReadableAmount := toHumanReadableAmount(event.Amount)
		fmt.Printf("%s: %s USDC from %s to %s (Block: %d)\n",
			event.Type, humanReadableAmount, event.From.Hex(), event.To.Hex(), event.BlockNumber)
	}
}

// CalculateTotalAmount calculates and returns the total amount of USDC transferred.
// It also prints the total amount to stdout.
func CalculateTotalAmount(events []models.EventLog) string {
	totalAmount := big.NewInt(0)
	for _, event := range events {
		totalAmount.Add(totalAmount, event.Amount)
	}

	total := toHumanReadableAmount(totalAmount)
	fmt.Printf("Total USDC transferred: %s\n", total)
	return total
}

// CalculateTotalAmountPerAddress calculates and prints the total amount of USDC transferred per address.
func CalculateTotalAmountPerAddress(events []models.EventLog) {
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
		fmt.Printf("Total USDC transferred from/to %s: %s\n", address, humanReadableAmount)
	}
}
