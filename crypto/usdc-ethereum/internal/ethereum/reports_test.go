package ethereum

import (
	"math/big"
	"testing"
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
