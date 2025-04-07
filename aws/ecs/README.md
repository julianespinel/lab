# Simple Express Application

A simple Express application with the following endpoints:
- `/healthcheck` - Returns the server status
- `/hello` - Returns a greeting message
- `/authors` - Returns a list of authors
- `/authors/:id` - Returns a specific author by ID

## Setup

```bash
# Install dependencies
npm install
```

## Running the Application

### Using Node.js directly
```bash
# Start the server
npm start
```

### Using Docker
```bash
# Build and start with Docker Compose
docker compose up -d

# View logs
docker compose logs -f

# Stop the container
docker compose down
```

### Using Docker manually
```bash
# Build the Docker image
docker build -t simple-express-app .

# Run the container
docker run -p 3000:3000 -d simple-express-app
```

The server will start on port 3000 by default. You can change this by setting the PORT environment variable.

## Database

The application uses PostgreSQL for data storage. When running with Docker, the database is automatically set up with the following:

- Migrations: The `authors` table is created automatically
- Seeds: 5 sample authors are added to the database

## Endpoints

### GET /healthcheck
Returns the status of the server and current timestamp.

Example response:
```json
{
  "status": "ok",
  "timestamp": "2023-07-25T12:34:56.789Z"
}
```

### GET /hello
Returns a greeting message.

Example response:
```json
{
  "message": "Hello, world!"
}
```

### GET /authors
Returns a list of all authors.

Example response:
```json
[
  {
    "id": 1,
    "name": "Jane Austen",
    "email": "jane.austen@example.com",
    "bio": "English novelist known primarily for her six major novels...",
    "created_at": "2023-07-25T12:34:56.789Z",
    "updated_at": "2023-07-25T12:34:56.789Z"
  },
  // ... more authors
]
```

### GET /authors/:id
Returns a specific author by ID.

Example response:
```json
{
  "id": 1,
  "name": "Jane Austen",
  "email": "jane.austen@example.com",
  "bio": "English novelist known primarily for her six major novels...",
  "created_at": "2023-07-25T12:34:56.789Z",
  "updated_at": "2023-07-25T12:34:56.789Z"
}
```
