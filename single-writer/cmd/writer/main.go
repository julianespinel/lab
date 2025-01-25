package main

import (
	"context"
	"log"
	"time"
	"github.com/yourusername/lab/single-writer/internal/storage"
)

func main() {
	redisStore, err := storage.NewRedisStore("localhost:6379")
	if err != nil {
		log.Fatal(err)
	}

	sqliteStore, err := storage.NewSQLiteStore("votes.db")
	if err != nil {
		log.Fatal(err)
	}

	ctx := context.Background()
	
	for {
		vote, err := redisStore.GetNextVote(ctx)
		if err != nil {
			log.Printf("Error getting vote: %v", err)
			time.Sleep(time.Second)
			continue
		}
		
		if vote == nil {
			time.Sleep(100 * time.Millisecond)
			continue
		}

		// Update Redis totals
		if err := redisStore.IncrementVoteTotal(ctx, vote.Option); err != nil {
			log.Printf("Error incrementing vote total: %v", err)
			continue
		}

		// Save to SQLite
		if err := sqliteStore.SaveVote(vote); err != nil {
			log.Printf("Error saving vote: %v", err)
			continue
		}

		log.Printf("Processed vote for option: %s", vote.Option)
	}
} 