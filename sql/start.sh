# Open SQLite and create a new database file named my_database.db
sqlite3 my_database.db

# In SQLite, run the table creation SQL file
.read create_tables.sql

# Run the insert data SQL file to populate the tables
.read insert_data.sql
