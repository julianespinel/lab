package models

import (
	"math/big"
	"time"

	"github.com/ethereum/go-ethereum/common"
)

// ContractEvent represents an Ethereum event in a contract.
type ContractEvent struct {
	Type        string
	From        common.Address
	To          common.Address
	Amount      *big.Int
	BlockNumber uint64
	Timestamp   time.Time
}
