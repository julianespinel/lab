-- Create User table (main identity)
CREATE TABLE User (
                      id INT PRIMARY KEY,
                      username VARCHAR(50) NOT NULL,
                      email VARCHAR(100) UNIQUE NOT NULL
);

-- Create UserOrder table (one-to-many relation with User)
CREATE TABLE UserOrder (
                       id INT PRIMARY KEY,
                       user_id INT,
                       date DATE,
                       amount DECIMAL(10, 2),
                       FOREIGN KEY (user_id) REFERENCES User(id)
);

-- Create Profile table (one-to-one relation with User)
CREATE TABLE Profile (
                         id INT PRIMARY KEY,
                         user_id INT UNIQUE,
                         bio TEXT,
                         profile_picture_url VARCHAR(255),
                         FOREIGN KEY (user_id) REFERENCES User(id)
);

-- Create Address table (one-to-one relation with User)
CREATE TABLE Address (
                         id INT PRIMARY KEY,
                         user_id INT UNIQUE,
                         street_address VARCHAR(255),
                         city VARCHAR(100),
                         postal_code VARCHAR(20),
                         FOREIGN KEY (user_id) REFERENCES User(id)
);

-- Create PaymentInfo table (one-to-one relation with User)
CREATE TABLE PaymentInfo (
                             id INT PRIMARY KEY,
                             user_id INT UNIQUE,
                             card_number VARCHAR(16),
                             expiry_date DATE,
                             cvv VARCHAR(3),
                             FOREIGN KEY (user_id) REFERENCES User(id)
);
