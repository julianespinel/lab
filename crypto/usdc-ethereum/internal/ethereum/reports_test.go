package ethereum

import (
	"math/big"
	"testing"

	"github.com/ethereum/go-ethereum/common"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/ethereum/models"
)

// Helper function to make assertions more readable
func assertStringEquals(t *testing.T, got, want string) {
	t.Helper()
	if got != want {
		t.Fatalf("want: %s, got: %s", want, got)
	}
}

// Helper function to make assertions more readable
func assertBigIntEquals(t *testing.T, got *big.Int, want *big.Int, msg string) {
	t.Helper()
	if got.Cmp(want) != 0 {
		t.Fatalf("%s: want %s, got %s", msg, want.String(), got.String())
	}
}

// addressFromHex creates an Ethereum address from a hex string.
// It panics if the address is invalid.
func addressFromHex(hex string) common.Address {
	return common.HexToAddress(hex)
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
	assertBigIntEquals(t, result, big.NewInt(0), "total amount")
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
	assertBigIntEquals(t, result, big.NewInt(1000000), "total amount")
}

func TestCalculateTotalAmount_MultipleEvents_ReturnsSum(t *testing.T) {
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
	assertBigIntEquals(t, result, big.NewInt(1500001500000), "total amount")
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
	assertBigIntEquals(t, result, big.NewInt(3), "total amount")
}

func TestCalculateTotalAmountPerAddress_EmptyEvents_ReturnsEmptyMap(t *testing.T) {
	// Arrange
	events := []models.EventLog{}

	// Act
	result := CalculateTotalAmountPerAddress(events)

	// Assert
	if len(result) != 0 {
		t.Errorf("expected empty map, got map with %d entries", len(result))
	}
}

func TestCalculateTotalAmountPerAddress_SingleTransfer_CorrectBalances(t *testing.T) {
	// Arrange
	sender := "0x1111111111111111111111111111111111111111"
	receiver := "0x2222222222222222222222222222222222222222"
	events := []models.EventLog{
		{
			From:   addressFromHex(sender),
			To:     addressFromHex(receiver),
			Amount: big.NewInt(1000000), // 1 USDC
		},
	}

	// Act
	result := CalculateTotalAmountPerAddress(events)

	// Assert
	assertBigIntEquals(t, result[sender], big.NewInt(-1000000), "sender balance")
	assertBigIntEquals(t, result[receiver], big.NewInt(1000000), "receiver balance")
}

func TestCalculateTotalAmountPerAddress_MultipleTransfers_CorrectBalances(t *testing.T) {
	// Arrange
	addr1 := "0x1111111111111111111111111111111111111111"
	addr2 := "0x2222222222222222222222222222222222222222"
	addr3 := "0x3333333333333333333333333333333333333333"
	events := []models.EventLog{
		{
			From:   addressFromHex(addr1),
			To:     addressFromHex(addr2),
			Amount: big.NewInt(2000000), // 2 USDC
		},
		{
			From:   addressFromHex(addr2),
			To:     addressFromHex(addr3),
			Amount: big.NewInt(1000000), // 1 USDC
		},
	}

	// Act
	result := CalculateTotalAmountPerAddress(events)

	// Assert
	assertBigIntEquals(t, result[addr1], big.NewInt(-2000000), "addr1 balance")
	assertBigIntEquals(t, result[addr2], big.NewInt(1000000), "addr2 balance")
	assertBigIntEquals(t, result[addr3], big.NewInt(1000000), "addr3 balance")
}

func TestCalculateTotalAmountPerAddress_CircularTransfer_CorrectBalances(t *testing.T) {
	// Arrange
	addr1 := "0x1111111111111111111111111111111111111111"
	addr2 := "0x2222222222222222222222222222222222222222"
	events := []models.EventLog{
		{
			From:   addressFromHex(addr1),
			To:     addressFromHex(addr2),
			Amount: big.NewInt(1000000), // 1 USDC
		},
		{
			From:   addressFromHex(addr2),
			To:     addressFromHex(addr1),
			Amount: big.NewInt(1000000), // 1 USDC back
		},
	}

	// Act
	result := CalculateTotalAmountPerAddress(events)

	// Assert
	assertBigIntEquals(t, result[addr1], big.NewInt(0), "addr1 balance")
	assertBigIntEquals(t, result[addr2], big.NewInt(0), "addr2 balance")
}
