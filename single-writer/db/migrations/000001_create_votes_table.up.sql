CREATE TABLE votes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    option TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
); 