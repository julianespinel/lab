const { dbConfig } = require('../config/database');
const knex = require('knex')(dbConfig);

async function runSeeds() {
  try {
    console.log('Running seeds...');
    await knex.seed.run();
    console.log('Seeds completed successfully');
  } catch (error) {
    console.error('Seeding failed:', error);
    process.exit(1);
  } finally {
    await knex.destroy();
  }
}

runSeeds();
