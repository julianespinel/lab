package storage

import (
	"database/sql"

	"github.com/golang-migrate/migrate/v4"
	"github.com/golang-migrate/migrate/v4/database/sqlite3"
	_ "github.com/golang-migrate/migrate/v4/source/file"
	_ "github.com/mattn/go-sqlite3" // SQLite driver
	"github.com/yourusername/lab/single-writer/internal/models"
)

type SQLiteStore struct {
	db *sql.DB
}

func NewSQLiteStore(dbPath string) (*SQLiteStore, error) {
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return nil, err
	}

	// Run migrations
	driver, err := sqlite3.WithInstance(db, &sqlite3.Config{})
	if err != nil {
		return nil, err
	}

	m, err := migrate.NewWithDatabaseInstance(
		"file://db/migrations",
		"sqlite3",
		driver,
	)
	if err != nil {
		return nil, err
	}

	if err := m.Up(); err != nil && err != migrate.ErrNoChange {
		return nil, err
	}

	return &SQLiteStore{db: db}, nil
}

func (s *SQLiteStore) SaveVote(vote *models.Vote) error {
	tx, err := s.db.Begin()
	if err != nil {
		return err
	}
	defer tx.Rollback()

	// Insert the vote
	result, err := tx.Exec(
		"INSERT INTO votes (option, created_at) VALUES (?, ?)",
		vote.Option, vote.CreatedAt,
	)
	if err != nil {
		return err
	}

	vote.ID, _ = result.LastInsertId()
	return tx.Commit()
}

// GetVoteByID retrieves a vote by its ID
func (s *SQLiteStore) GetVoteByID(id int64) (*models.Vote, error) {
	var vote models.Vote
	err := s.db.QueryRow(
		"SELECT id, option, created_at FROM votes WHERE id = ?",
		id,
	).Scan(&vote.ID, &vote.Option, &vote.CreatedAt)

	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}

	return &vote, nil
}

// GetVoteTotals retrieves vote totals from SQLite
func (s *SQLiteStore) GetVoteTotals() (map[string]int64, error) {
	rows, err := s.db.Query(`
		SELECT option, COUNT(*) as count 
		FROM votes 
		GROUP BY option
	`)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	totals := make(map[string]int64)
	for rows.Next() {
		var option string
		var count int64
		if err := rows.Scan(&option, &count); err != nil {
			return nil, err
		}
		totals[option] = count
	}
	return totals, rows.Err()
}
