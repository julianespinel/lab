package main

import (
	"context"
	"log"
	"net/http"

	"github.com/gorilla/mux"
	"github.com/yourusername/lab/single-writer/internal/api"
	"github.com/yourusername/lab/single-writer/internal/storage"
)

// refreshRedisCache initializes Redis cache with data from SQLite
func refreshRedisCache(redisStore *storage.RedisStore, sqliteStore *storage.SQLiteStore) error {
	totals, err := sqliteStore.GetVoteTotals()
	if err != nil {
		return err
	}
	return redisStore.SetVoteTotals(context.Background(), totals)
}

func main() {
	sqliteStore, err := storage.NewSQLiteStore("votes.db")
	if err != nil {
		log.Fatal(err)
	}

	redisStore, err := storage.NewRedisStore("localhost:6379")
	if err != nil {
		log.Fatal(err)
	}

	// Refresh Redis cache from SQLite on startup
	if err := refreshRedisCache(redisStore, sqliteStore); err != nil {
		log.Fatal(err)
	}

	handler := api.NewHandler(redisStore, sqliteStore)

	r := mux.NewRouter()
	r.HandleFunc("/votes", handler.HandleCreateVote).Methods("POST")
	r.HandleFunc("/votes", handler.HandleGetVoteTotals).Methods("GET")
	r.HandleFunc("/votes/{id:[0-9]+}", handler.HandleGetVote).Methods("GET")

	log.Println("Starting API server on :8080")
	if err := http.ListenAndServe(":8080", r); err != nil {
		log.Fatal(err)
	}
}
