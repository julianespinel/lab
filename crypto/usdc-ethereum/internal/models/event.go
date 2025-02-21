package models

import (
	"math/big"
	"time"

	"github.com/ethereum/go-ethereum/common"
)

// Event represents an Ethereum log in a contract or in a wallet transaction.
type Event struct {
	Type        string
	From        common.Address
	To          common.Address
	Amount      *big.Int
	BlockNumber uint64
	Timestamp   time.Time
}
