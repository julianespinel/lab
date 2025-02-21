package usdc

import (
	"encoding/json"
	"fmt"
	"math/big"
	"net/http"
	"strconv"
	"time"

	"github.com/ethereum/go-ethereum/common"
	"github.com/julianespinel/lab/crypto/usdc-ethereum/pkg/models"
)

type EtherscanResponse struct {
	Status  string `json:"status"`
	Message string `json:"message"`
	Result  []struct {
		BlockNumber string `json:"blockNumber"`
		TimeStamp   string `json:"timeStamp"`
		Hash        string `json:"hash"`
		From        string `json:"from"`
		To          string `json:"to"`
		Value       string `json:"value"`
	} `json:"result"`
}

func FetchTransactionsFromEtherscan(apiKey, walletAddress string, numTransactions int) ([]models.ContractEvent, error) {
	url := fmt.Sprintf("https://api.etherscan.io/api?module=account&action=tokentx&address=%s&startblock=0&endblock=99999999&sort=desc&apikey=%s", walletAddress, apiKey)

	resp, err := http.Get(url)
	if err != nil {
		return nil, fmt.Errorf("error fetching transactions: %v", err)
	}
	defer resp.Body.Close()

	var etherscanResponse EtherscanResponse
	if err := json.NewDecoder(resp.Body).Decode(&etherscanResponse); err != nil {
		return nil, fmt.Errorf("error decoding response: %v", err)
	}

	if etherscanResponse.Status != "1" {
		return nil, fmt.Errorf("error from Etherscan: %s", etherscanResponse.Message)
	}

	var events []models.ContractEvent
	for i, tx := range etherscanResponse.Result {
		if i >= numTransactions {
			break
		}

		blockNumber, err := strconv.ParseUint(tx.BlockNumber, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("error parsing block number: %v", err)
		}

		timeStamp, err := strconv.ParseInt(tx.TimeStamp, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("error parsing timestamp: %v", err)
		}

		amount, success := new(big.Int).SetString(tx.Value, 10)
		if !success {
			return nil, fmt.Errorf("error parsing transaction value: %s", tx.Value)
		}

		event := models.ContractEvent{
			Type:        "TOKEN_TRANSFER",
			From:        common.HexToAddress(tx.From),
			To:          common.HexToAddress(tx.To),
			Amount:      amount,
			BlockNumber: blockNumber,
			Timestamp:   time.Unix(timeStamp, 0),
		}
		events = append(events, event)
	}

	return events, nil
}
