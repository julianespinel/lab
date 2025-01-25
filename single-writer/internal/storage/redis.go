package storage

import (
	"context"
	"encoding/json"
	"strconv"

	"github.com/go-redis/redis/v8"
	"github.com/yourusername/lab/single-writer/internal/models"
)

const (
	VotesQueue  = "votes:queue"
	VotesTotals = "votes:totals"
)

type RedisStore struct {
	client *redis.Client
}

func NewRedisStore(addr string) (*RedisStore, error) {
	client := redis.NewClient(&redis.Options{
		Addr: addr,
	})

	// Test connection
	ctx := context.Background()
	if err := client.Ping(ctx).Err(); err != nil {
		return nil, err
	}

	return &RedisStore{client: client}, nil
}

func (s *RedisStore) QueueVote(ctx context.Context, vote *models.Vote) error {
	data, err := json.Marshal(vote)
	if err != nil {
		return err
	}

	return s.client.RPush(ctx, VotesQueue, data).Err()
}

func (s *RedisStore) GetNextVote(ctx context.Context) (*models.Vote, error) {
	data, err := s.client.LPop(ctx, VotesQueue).Bytes()
	if err == redis.Nil {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}

	var vote models.Vote
	if err := json.Unmarshal(data, &vote); err != nil {
		return nil, err
	}

	return &vote, nil
}

func (s *RedisStore) IncrementVoteTotal(ctx context.Context, option string) error {
	return s.client.HIncrBy(ctx, VotesTotals, option, 1).Err()
}

// GetVoteTotals returns a map of option -> count
func (s *RedisStore) GetVoteTotals(ctx context.Context) (map[string]int64, error) {
	result := make(map[string]int64)
	totals, err := s.client.HGetAll(ctx, VotesTotals).Result()
	if err != nil {
		return nil, err
	}

	for option, count := range totals {
		countInt, _ := strconv.ParseInt(count, 10, 64)
		result[option] = countInt
	}

	return result, nil
}

// SetVoteTotals caches vote totals in Redis
func (s *RedisStore) SetVoteTotals(ctx context.Context, totals map[string]int64) error {
	// Use a pipeline to set all values atomically
	pipe := s.client.Pipeline()

	// First, clear existing totals
	pipe.Del(ctx, VotesTotals)

	// Then set new totals
	for option, count := range totals {
		pipe.HSet(ctx, VotesTotals, option, count)
	}

	_, err := pipe.Exec(ctx)
	return err
}
