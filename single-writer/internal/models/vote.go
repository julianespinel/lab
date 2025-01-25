package models

import "time"

type Vote struct {
    ID        int64     `json:"id"`
    Option    string    `json:"option"`
    CreatedAt time.Time `json:"created_at"`
}

type VoteRequest struct {
    Option string `json:"option"`
} 