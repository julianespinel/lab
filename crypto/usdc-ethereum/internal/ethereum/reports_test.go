package ethereum

import (
	"math/big"
	"testing"

	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
)

// Helper function to make assertions more readable
func assertStringEquals(t *testing.T, got, want string) {
	t.Helper()
	if got != want {
		t.Fatalf("want: %s, got: %s", want, got)
	}
}

func TestToHumanReadableAmount_OneUSDC_Returns1Point0(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1000000) // 1 USDC = 1000000 units

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertStringEquals(t, result, "1.000000")
}

func TestToHumanReadableAmount_ZeroUSDC_Returns0Point0(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(0)

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertStringEquals(t, result, "0.000000")
}

func TestToHumanReadableAmount_HalfUSDC_Returns0Point5(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(500000) // 0.5 USDC

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertStringEquals(t, result, "0.500000")
}

func TestToHumanReadableAmount_LargeAmount_ReturnsWithCommas(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1000000000000) // 1,000,000 USDC

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertStringEquals(t, result, "1,000,000.000000")
}

func TestToHumanReadableAmount_VeryLargeAmount_ReturnsWithCommas(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1234567000000) // 1,234,567 USDC

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertStringEquals(t, result, "1,234,567.000000")
}

func TestToHumanReadableAmount_SmallestUnit_ReturnsSmallestDecimal(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1) // 0.000001 USDC (smallest unit)

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertStringEquals(t, result, "0.000001")
}

func TestCalculateTotalAmount_EmptyEvents_ReturnsZero(t *testing.T) {
	// Arrange
	events := []models.EventLog{}

	// Act
	result := CalculateTotalAmount(events)

	// Assert
	assertStringEquals(t, result, "0.000000")
}

func TestCalculateTotalAmount_SingleEvent_ReturnsSameAmount(t *testing.T) {
	// Arrange
	events := []models.EventLog{
		{
			Amount: big.NewInt(1000000), // 1 USDC
		},
	}

	// Act
	result := CalculateTotalAmount(events)

	// Assert
	assertStringEquals(t, result, "1.000000")
}

func TestCalculateTotalAmount_MultipleEvents_ReturnsSumWithCommas(t *testing.T) {
	// Arrange
	events := []models.EventLog{
		{
			Amount: big.NewInt(1000000000000), // 1,000,000 USDC
		},
		{
			Amount: big.NewInt(500000000000), // 500,000 USDC
		},
		{
			Amount: big.NewInt(1500000), // 1.5 USDC
		},
	}

	// Act
	result := CalculateTotalAmount(events)

	// Assert
	assertStringEquals(t, result, "1,500,001.500000")
}

func TestCalculateTotalAmount_SmallAmounts_ReturnsSum(t *testing.T) {
	// Arrange
	events := []models.EventLog{
		{
			Amount: big.NewInt(1), // 0.000001 USDC
		},
		{
			Amount: big.NewInt(2), // 0.000002 USDC
		},
	}

	// Act
	result := CalculateTotalAmount(events)

	// Assert
	assertStringEquals(t, result, "0.000003")
}
