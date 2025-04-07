const express = require('express');
const { db } = require('./config/database');
const Author = require('./models/Author');
const path = require('path');

// Initialize Express app
const app = express();
const PORT = process.env.PORT || 3000;

app.use(express.json());

// Initialize database and run migrations and seeds
async function initializeDatabase() {
  const maxRetries = 10;
  let retries = 0;
  let lastError;

  while (retries < maxRetries) {
    try {
      // Check database connection
      await db.raw('SELECT 1');
      console.log('Database connection successful');

      // Run migrations manually
      console.log('Running migrations...');
      const migrationPath = path.join(__dirname, 'migrations', '20240407_create_authors_table.js');
      const migration = require(migrationPath);
      await migration.up(db);
      console.log('Migrations completed successfully');

      // Run seeds manually - only if there are no authors yet
      console.log('Checking for existing authors...');
      const authorCount = await db('authors').count('id as count').first();

      if (!authorCount || authorCount.count === '0') {
        console.log('No authors found, running seeds...');
        const seedsPath = path.join(__dirname, 'seeds', 'authors.js');
        const seeds = require(seedsPath);
        await seeds.seed(db);
        console.log('Seeds completed successfully');
      } else {
        console.log(`Found ${authorCount.count} existing authors, skipping seeds`);
      }

      return;
    } catch (error) {
      retries++;
      lastError = error;
      console.error(`Database connection attempt ${retries}/${maxRetries} failed:`, error.message);

      if (retries < maxRetries) {
        const waitTime = Math.min(retries * 2000, 20000); // Increase wait time with each retry, max 20 seconds
        console.log(`Waiting ${waitTime}ms before retrying...`);
        await new Promise(resolve => setTimeout(resolve, waitTime));
      }
    }
  }

  console.error('Database initialization failed after retries:', lastError);
  process.exit(1);
}

// Healthcheck endpoint - return OK even if DB is not ready
app.get('/healthcheck', (req, res) => {
  res.status(200).json({
    status: 'ok',
    timestamp: new Date().toISOString()
  });
});

// Hello endpoint
app.get('/hello', (req, res) => {
  res.status(200).json({
    message: 'Hello, world!'
  });
});

// Authors endpoint
app.get('/authors', async (req, res) => {
  try {
    const authors = await Author.getAll();
    res.status(200).json(authors);
  } catch (error) {
    console.error('Error fetching authors:', error);
    res.status(500).json({ error: 'Failed to fetch authors' });
  }
});

// Get author by ID
app.get('/authors/:id', async (req, res) => {
  try {
    const author = await Author.getById(req.params.id);
    if (!author) {
      return res.status(404).json({ error: 'Author not found' });
    }
    res.status(200).json(author);
  } catch (error) {
    console.error('Error fetching author:', error);
    res.status(500).json({ error: 'Failed to fetch author' });
  }
});

// Start the server right away, initialize the database in the background
app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);

  // Initialize the database in the background
  initializeDatabase().catch(error => {
    console.error('Failed to initialize database:', error);
    // Don't exit the process, as we want the healthcheck endpoint to remain available
  });
});
