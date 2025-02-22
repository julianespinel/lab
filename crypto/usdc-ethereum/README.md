# Ethereum Blockchain Query

This project demonstrates how to query contracts and wallets in the Ethereum blockchain.

We are doing the following two main operations:

**1. Fetching events from a contract**

We are fetching events from the USDC contract given a range of dates.
Currently, we are calculating the blocks that contain the transactions for the given range of dates by using the approximation of one block every 12 seconds.

- A more precise way of doing this is to search the blocks by using binary search.
- However, this approach is rate limited by the Ethereum API provider (in this case Infura). That's why we are using the 12 seconds approximation.

After we have the range of blocks, we fetch the blocks within that range using batches of N blocks until we complete the range.

For each batch, we fetch the events from the contract and process them to extract the logs of each event. From each log, we create an `EventLog` model (a struct we created to represent the event log), and then we append it to a list of `EventLog`. This list is returned at the end of the function.

Finally, we take the list of `EventLog` and process them to calculate the total amount of USDC transferred, and the total amount of USDC transferred per address within the given range of dates.

**2. Fetching transactions from a wallet**

We are fetching the last N transactions from a wallet given a wallet address.

Since we are interested in the USDC transactions, we are using the Etherscan API to fetch the ERC-20 token events (associated to a wallet) directly without having to scan the blockchain. Then, we convert the Etherscan transactions to our `EventLog` model, and return a list of `EventLog`.

Finally, we take the list of `EventLog` and process them to calculate the total amount of USDC transferred (to/from the wallet), and the total amount of USDC transferred per address (to/from the wallet) within the given range of dates.

## Things I would like to improve/try

1. Install an Ethereum node and use it to fetch the transactions instead of using the Infura or Etherscan API. (to avoid rate limiting).
2. Use binary search to calculate the range of blocks more precisely.
3. Use an iterator pattern to isolate the logic related to the pagination when fetching:
   * The range of blocks
   * The events from a contract
   * The transactions from a wallet

## How to run

1. Set the environment variables:
  ```
  export INFURA_API_KEY=<your-infura-api-key>
  export ETHERSCAN_API_KEY=<your-etherscan-api-key>
  ```
2. Run `make run`

## How to test

1. Run `make test`

## Brain dump

This section describes my understanding of the Ethereum blockchain and how to interact with it.

### Ethereum Contracts

An Ethereum contract is a collection of code (functions) and data (state) that resides at a specific address on the Ethereum blockchain.

An Ethereum contract can have transactions. In the case of the USDC contract, those transactions are for 0 ETH. Why?
* Because the [USDC contract](https://etherscan.io/address/0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48) is a ERC20 contract.
* It manages USDC balances.
* It transfers USDC tokens, not ETH
  * By using smart contract function calls, e.g: `transfer(address, uint256)`


This is an example of a transaction for the USDC contract: https://etherscan.io/tx/0x222902aa4b4dbb38ac4c342b18a60237fbdb512dda01092ee5c588f328424472

As you can see, the transaction is for 0 ETH. But if you look at the [transaction logs](https://etherscan.io/tx/0x222902aa4b4dbb38ac4c342b18a60237fbdb512dda01092ee5c588f328424472#eventlog), you can see that there is an event for the transfer of 99.02 USDC from address A to address B.

### Ethereum Wallets

A wallet is an address that contains a balance of ETH.
A wallet does not hold ERC-20 tokens directly.
ERC-20 tokens balances are tracked by the respective ERC-20 contract.

An Ethereum transaction is different from a USDC transaction.

A USDC transaction is represented as an ERC20 event.
See: https://etherscan.io/tx/0x06ea48bf428a77dfb18e96cbf7746bfbcd355ddaf6398933154af82c36ac614a
* The transaction is for 0 ETH
* The [transaction logs](https://etherscan.io/tx/0x06ea48bf428a77dfb18e96cbf7746bfbcd355ddaf6398933154af82c36ac614a#eventlog) have an event for the transfer of 1,105 USDC from address A to address B.

### ETH vs ERC-20 Transfers

* ETH transfers directly modify the recipient's balance.
* ERC-20 transfers modify the contract's internal ledger (not the Ethereum base layer).
* ERC-20 transfers emit logs so clients (wallets, explorers) can track balances.

#### Why Are USDC Transactions "0 ETH"?

* Calling `transfer(address, uint256)` on an ERC-20 contract only changes balances inside the smart contract.
* No ETH is transferred because ETH is separate from ERC-20 tokens.

#### How Do Wallets Track Token Balances?

* Wallets don't natively "hold" tokens; instead, they query ERC-20 contracts to check balances.
* Example: The USDC contract has a `balanceOf(address)` function that returns the balance for a given wallet.


## Project structure

* `cmd/main.go`: Main entry point for the project.

There is one main package: `internal/ethereum/`. The goal of this package is to abstract the interaction we need to have with the Ethereum blockchain.

This package has the following dependencies:
* `clients/`: Contains the logic for querying the Ethereum blockchain.
   * In particular, we are using Infura (go-ethereum) and Etherscan (HTTP).
* `config/`: Contains the logic for loading the configuration from a YAML file.
* `blocks/`: Contains the logic for querying blocks in the Ethereum blockchain.
* `logs/`: Contains the logic for fetching and parsing event logs from the Ethereum blockchain.
* `models/`: Contains the models we defined to represent the data we get from the Ethereum blockchain.
* `service.go`: Contains the business logic we want to expose to other packages.
* `report.go`: Contains the reports we want to expose to other packages.

The Ethereum package should expose functions only from `service.go` and `report.go`. All the other files have private methods only. Why?
* Because we want to encapsulate the logic we use to interact with the Ethereum blockchain.
* Because we want to provide a simple interface that satisfies business needs.

### How to support another blockchain

Let's say we want to support Solana:

* We would need to create a new package, e.g., `solana/`.

Within the `solana/` package, we would need to create the following directories:

* `clients/`: Contains the logic for querying the Solana blockchain.
* `config/`: Contains the logic for loading the configuration from a YAML file.
* `blocks/`: Contains the logic for querying blocks in the Solana blockchain.
* `logs/`: Contains the logic for fetching and parsing event logs from the Solana blockchain. (If that concept exists in Solana)
* `models/`: Contains the models we defined to represent the data we get from the Solana blockchain.
* `service.go`: Contains the business logic we want to expose to other packages.
* `report.go`: Contains the reports we want to expose to other packages.

The `solana/` package should be self-contained. It should not depend on the Ethereum package.
