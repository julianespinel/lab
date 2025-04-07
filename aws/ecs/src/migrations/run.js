const { dbConfig } = require('../config/database');
const knex = require('knex')(dbConfig);

async function runMigrations() {
  try {
    console.log('Running migrations...');
    await knex.migrate.latest();
    console.log('Migrations completed successfully');
  } catch (error) {
    console.error('Migration failed:', error);
    process.exit(1);
  } finally {
    await knex.destroy();
  }
}

runMigrations();
