package api

import (
	"encoding/json"
	"net/http"
	"strconv"
	"time"

	"github.com/yourusername/lab/single-writer/internal/models"
	"github.com/yourusername/lab/single-writer/internal/storage"
)

type Handler struct {
	redisStore  *storage.RedisStore
	sqliteStore *storage.SQLiteStore
}

func NewHandler(redisStore *storage.RedisStore, sqliteStore *storage.SQLiteStore) *Handler {
	return &Handler{
		redisStore:  redisStore,
		sqliteStore: sqliteStore,
	}
}

func (h *Handler) HandleVote(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var voteReq models.VoteRequest
	if err := json.NewDecoder(r.Body).Decode(&voteReq); err != nil {
		http.Error(w, "Invalid request body", http.StatusBadRequest)
		return
	}

	vote := &models.Vote{
		Option:    voteReq.Option,
		CreatedAt: time.Now(),
	}

	if err := h.redisStore.QueueVote(r.Context(), vote); err != nil {
		http.Error(w, "Failed to queue vote", http.StatusInternalServerError)
		return
	}

	w.WriteHeader(http.StatusAccepted)
	json.NewEncoder(w).Encode(vote)
}

// HandleGetTotals returns the total votes for each option
func (h *Handler) HandleGetTotals(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Try Redis first
	totals, err := h.redisStore.GetVoteTotals(r.Context())
	if err != nil || len(totals) == 0 {
		// Fall back to SQLite if Redis fails or returns empty results
		totals, err = h.sqliteStore.GetVoteTotals()
		if err != nil {
			http.Error(w, "Failed to get vote totals", http.StatusInternalServerError)
			return
		}

		// Cache the results in Redis
		if err := h.redisStore.SetVoteTotals(r.Context(), totals); err != nil {
			// Log the error but continue since we still have the data from SQLite
			// In a production system, we'd want to properly log this error
			println("Failed to cache vote totals in Redis:", err.Error())
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(totals)
}

// HandleGetVote returns a specific vote by ID
func (h *Handler) HandleGetVote(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Get vote ID from URL parameter
	idStr := r.URL.Query().Get("id")
	id, err := strconv.ParseInt(idStr, 10, 64)
	if err != nil {
		http.Error(w, "Invalid vote ID", http.StatusBadRequest)
		return
	}

	vote, err := h.sqliteStore.GetVoteByID(id)
	if err != nil {
		http.Error(w, "Failed to get vote", http.StatusInternalServerError)
		return
	}
	if vote == nil {
		http.Error(w, "Vote not found", http.StatusNotFound)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(vote)
}
