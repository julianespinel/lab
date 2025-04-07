/**
 * @param { import("knex").Knex } knex
 * @returns { Promise<void> }
 */
exports.seed = async function(knex) {
  // Deletes ALL existing entries
  await knex('authors').del();

  // Insert 5 authors
  await knex('authors').insert([
    {
      name: 'Jane Austen',
      email: 'jane.austen@example.com',
      bio: 'English novelist known primarily for her six major novels, which interpret, critique and comment upon the British landed gentry at the end of the 18th century.'
    },
    {
      name: 'George Orwell',
      email: 'george.orwell@example.com',
      bio: 'English novelist, essayist, journalist and critic. His work is characterised by lucid prose, social criticism, opposition to totalitarianism, and support of democratic socialism.'
    },
    {
      name: 'Gabriel García Márquez',
      email: 'gabriel.marquez@example.com',
      bio: 'Colombian novelist, short-story writer, screenwriter, and journalist, known affectionately as Gabo or Gabito throughout Latin America.'
    },
    {
      name: 'Toni Morrison',
      email: 'toni.morrison@example.com',
      bio: 'American novelist, essayist, book editor, and college professor. Her first novel, The Bluest Eye, was published in 1970.'
    },
    {
      name: 'Haruki Murakami',
      email: 'haruki.murakami@example.com',
      bio: 'Japanese writer. His novels, essays, and short stories have been bestsellers in Japan as well as internationally, with his work translated into 50 languages.'
    }
  ]);
};
