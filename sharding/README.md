# Sharding

This is an experiment to understand how database sharding works.

In this experiment we created a system that store orders following these
conditions:

1. We define N number of shards.
   1. The different shards (databases) will be created locally using Docker.
1. Each order has a numeric ID.
   1. We will use that ID as our sharding key.

## Dependencies

In order to be able to install and run the program, please make sure
you have the following dependencies installed in your host:

1. Java
1. Maven
1. Docker

## Install

1. `make install`

## Test

1. `make test`

## Run

1. `make package`
1. We can execute the program with any of the following commands:
   1. `target/appassembler/bin/shard -s 3 -o 10000 -wts 10`: (3 shards, 10k orders, waiting time 10 seconds)
   1. `target/appassembler/bin/shard --shards 3 --orders 10000 --waiting-time-seconds 10`: (3 shards, 10k orders, waiting time 10 seconds)
   
To show all the possible parameters that we can execute:
```bash
target/appassembler/bin/shard -h
```

## Verify

If you want to see that data inserted in the different shards, please do the
following:

1. Execute the program with a high waiting time argument:
   1. For example 10 minutes: `sharding -s 3 -o 10000 -wts 600`
   1. After 10 minutes all the database shards will be stopped
1. List the containers of the Postgres database:
   1. `docker ps`
1. Connect to each container (database shard):
   1. `psql -h localhost -p <port> -U username -d sharding_lab_db`
   1. The ports range is: `[15432 to (15432 + numberOfShards - 1)]`
   1. The password is: `password`
1. Count the elements on each shard:
   1. `SELECT COUNT(*) FROM orders;`
