# USDC Contract Query in Ethereum Blockchain

This project demonstrates how to query a USDC contract in the Ethereum blockchain.

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

### Ethereum Contracts

An Ethereum contract is a collection of code (its functions) and data (its state) that resides at a specific address on the Ethereum blockchain.

An Ethereum contract can have transactions. In the case of the USDC contract, those transactions are for 0 ETH. Why?
* Because the [USDC contract](https://etherscan.io/address/0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48) is a ERC20 contract.
* It manages USDC balances.
* It transfers USDC tokens, not ETH
  * By trigger smart contract function calls, e.g., `transfer(address, uint256)`


This is an example of a transaction for the USDC contract: https://etherscan.io/tx/0x222902aa4b4dbb38ac4c342b18a60237fbdb512dda01092ee5c588f328424472

As you can see, the transaction is for 0 ETH. But if you look at the [transaction logs](https://etherscan.io/tx/0x222902aa4b4dbb38ac4c342b18a60237fbdb512dda01092ee5c588f328424472#eventlog), you can see that there is an event for the transfer of 99.02 USDC from address A to address B.

### Ethereum Wallets

A wallet is an address that contains a balance of ETH.
A wallet do not hold ERC-20 tokens directly.
ERC-20 tokens balances are tracked by the ERC-20 contract.

An Ethereum transaction is different from a USDC transaction.

A USDC transaction is represented as a ERC20 event.
See: https://etherscan.io/tx/0x06ea48bf428a77dfb18e96cbf7746bfbcd355ddaf6398933154af82c36ac614a
* The transaction is for 0 ETH
* The [transaction logs](https://etherscan.io/tx/0x06ea48bf428a77dfb18e96cbf7746bfbcd355ddaf6398933154af82c36ac614a#eventlog) have an event for the transfer of 1,105 USDC from address A to address B.

### ETH vs. ERC-20 Transfers

* ETH transfers directly modify the recipient’s balance.
* ERC-20 transfers modify the contract’s internal ledger (not the Ethereum base layer).
* ERC-20 transfers emit logs so clients (wallets, explorers) can track balances.

#### Why Are USDC Transactions "0 ETH"?

* Calling transfer(address, uint256) on an ERC-20 contract only changes balances inside the smart contract.
* No ETH is transferred because ETH is separate from ERC-20 tokens.

#### How Do Wallets Track Token Balances?

* Wallets don’t natively "hold" tokens; instead, they query ERC-20 contracts to check balances.
* Example: The USDC contract has a balanceOf(address) function that returns the balance for a given wallet.
