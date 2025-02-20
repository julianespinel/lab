PHONY: run start build clean test

run:
	go run cmd/main.go

start:
	./usdc-query
	 
build:
	go build -o usdc-query cmd/main.go

clean:
	rm -f usdc-query

test:
	go test -v ./...
