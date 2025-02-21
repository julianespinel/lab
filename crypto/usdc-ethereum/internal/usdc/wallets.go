package usdc

import (
	"fmt"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/clients"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/internal/models"
	"math/big"
	"strconv"
	"time"

	"github.com/ethereum/go-ethereum/common"
)

// createEventFromEtherscanTx converts an Etherscan transaction to a Event
func createEventFromEtherscanTx(tx clients.EtherscanTransaction) (models.Event, error) {
	blockNumber, err := strconv.ParseUint(tx.BlockNumber, 10, 64)
	if err != nil {
		return models.Event{}, fmt.Errorf("error parsing block number: %v", err)
	}

	timeStamp, err := strconv.ParseInt(tx.TimeStamp, 10, 64)
	if err != nil {
		return models.Event{}, fmt.Errorf("error parsing timestamp: %v", err)
	}

	amount, success := new(big.Int).SetString(tx.Value, 10)
	if !success {
		return models.Event{}, fmt.Errorf("error parsing transaction value: %s", tx.Value)
	}

	return models.Event{
		Type:        "TOKEN_TRANSFER",
		From:        common.HexToAddress(tx.From),
		To:          common.HexToAddress(tx.To),
		Amount:      amount,
		BlockNumber: blockNumber,
		Timestamp:   time.Unix(timeStamp, 0),
	}, nil
}
