package models

import (
	"math/big"
	"time"

	"github.com/ethereum/go-ethereum/common"
)

// EventLog represents an Ethereum event log in a contract or in a wallet transaction.
type EventLog struct {
	Type        string
	From        common.Address
	To          common.Address
	Amount      *big.Int
	BlockNumber uint64
	Timestamp   time.Time
}
