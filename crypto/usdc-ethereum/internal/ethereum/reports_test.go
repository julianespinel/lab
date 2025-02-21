package ethereum

import (
	"math/big"
	"testing"
)

// Helper function to make assertions more readable
func assertFloatEquals(t *testing.T, got *big.Float, want string) {
	t.Helper()
	if got.Text('f', 6) != want {
		t.Fatalf("want: %s, got: %s", want, got.Text('f', 6))
	}
}

func TestToHumanReadableAmount_OneUSDC_Returns1Point0(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1000000) // 1 USDC = 1000000 units

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertFloatEquals(t, result, "1.000000")
}

func TestToHumanReadableAmount_ZeroUSDC_Returns0Point0(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(0)

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertFloatEquals(t, result, "0.000000")
}

func TestToHumanReadableAmount_HalfUSDC_Returns0Point5(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(500000) // 0.5 USDC

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertFloatEquals(t, result, "0.500000")
}

func TestToHumanReadableAmount_LargeAmount_Returns1Million(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1000000000000) // 1,000,000 USDC

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertFloatEquals(t, result, "1000000.000000")
}

func TestToHumanReadableAmount_SmallestUnit_ReturnsSmallestDecimal(t *testing.T) {
	// Arrange
	rawAmount := big.NewInt(1) // 0.000001 USDC (smallest unit)

	// Act
	result := toHumanReadableAmount(rawAmount)

	// Assert
	assertFloatEquals(t, result, "0.000001")
}
