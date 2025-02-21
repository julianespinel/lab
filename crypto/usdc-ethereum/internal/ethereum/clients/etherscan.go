package clients

import (
	"encoding/json"
	"fmt"
	"net/http"
)

// EtherscanTransaction represents a token transfer transaction from Etherscan
type EtherscanTransaction struct {
	BlockNumber string `json:"blockNumber"`
	TimeStamp   string `json:"timeStamp"`
	Hash        string `json:"hash"`
	From        string `json:"from"`
	To          string `json:"to"`
	Value       string `json:"value"`
}

type etherscanResponse struct {
	Status  string                 `json:"status"`
	Message string                 `json:"message"`
	Result  []EtherscanTransaction `json:"result"`
}

// EtherscanClient handles interactions with the Etherscan API
type EtherscanClient struct {
	apiKey     string
	httpClient *http.Client
}

// NewEtherscanClient creates a new Etherscan client
func NewEtherscanClient(apiKey string) *EtherscanClient {
	return &EtherscanClient{
		apiKey:     apiKey,
		httpClient: &http.Client{},
	}
}

// FetchTokenTransactions fetches token transactions for a wallet address
func (c *EtherscanClient) FetchTokenTransactions(walletAddress string) ([]EtherscanTransaction, error) {
	url := fmt.Sprintf("https://api.etherscan.io/api?module=account&action=tokentx&address=%s&startblock=0&endblock=99999999&sort=desc&apikey=%s",
		walletAddress, c.apiKey)

	resp, err := c.httpClient.Get(url)
	if err != nil {
		return nil, fmt.Errorf("error fetching transactions: %v", err)
	}
	defer resp.Body.Close()

	var response etherscanResponse
	if err := json.NewDecoder(resp.Body).Decode(&response); err != nil {
		return nil, fmt.Errorf("error decoding response: %v", err)
	}

	if response.Status != "1" {
		return nil, fmt.Errorf("error from Etherscan: %s", response.Message)
	}

	return response.Result, nil
}
