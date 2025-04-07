const { db } = require('../config/database');

class Author {
  static async getAll() {
    return db('authors').select('*');
  }

  static async getById(id) {
    return db('authors').where({ id }).first();
  }

  static async create(authorData) {
    const [id] = await db('authors').insert(authorData).returning('id');
    return id;
  }

  static async update(id, authorData) {
    return db('authors').where({ id }).update(authorData);
  }

  static async delete(id) {
    return db('authors').where({ id }).del();
  }
}

module.exports = Author;
