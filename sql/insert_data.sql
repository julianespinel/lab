-- Insert sample data into User table
INSERT INTO User (id, username, email) VALUES (1, 'johndoe', 'john@example.com');

-- Insert sample data into Profile table (one-to-one with User)
INSERT INTO Profile (id, user_id, bio, profile_picture_url)
VALUES (1, 1, 'Hello, I am John!', 'http://example.com/johndoe.jpg');

-- Insert sample data into Address table (one-to-one with User)
INSERT INTO Address (id, user_id, street_address, city, postal_code)
VALUES (1, 1, '123 Main St', 'Anytown', '12345');

-- Insert sample data into PaymentInfo table (one-to-one with User)
INSERT INTO PaymentInfo (id, user_id, card_number, expiry_date, cvv)
VALUES (1, 1, '1234567890123456', '2025-12-31', '123');

-- Insert sample data into Order table (one-to-many with User)
INSERT INTO UserOrder (id, user_id, date, amount) VALUES (1, 1, '2024-01-01', 100.00);
INSERT INTO UserOrder (id, user_id, date, amount) VALUES (2, 1, '2024-02-15', 250.50);

-- Insert User 2 (has everything except a Profile)
INSERT INTO User (id, username, email) VALUES (2, 'janedoe', 'jane@example.com');

-- Insert Address for User 2
INSERT INTO Address (id, user_id, street_address, city, postal_code)
VALUES (2, 2, '456 Oak St', 'Othertown', '67890');

-- Insert PaymentInfo for User 2
INSERT INTO PaymentInfo (id, user_id, card_number, expiry_date, cvv)
VALUES (2, 2, '2345678901234567', '2026-05-31', '456');

-- Insert multiple orders for User 2
INSERT INTO UserOrder (id, user_id, date, amount) VALUES (3, 2, '2024-03-10', 150.75);
INSERT INTO UserOrder (id, user_id, date, amount) VALUES (4, 2, '2024-04-22', 320.60);

-- Insert User 3 (has everything except PaymentInfo)
INSERT INTO User (id, username, email) VALUES (3, 'mikesmith', 'mike@example.com');

-- Insert Profile for User 3
INSERT INTO Profile (id, user_id, bio, profile_picture_url)
VALUES (3, 3, 'Mike here, tech enthusiast!', 'http://example.com/mikesmith.jpg');

-- Insert Address for User 3
INSERT INTO Address (id, user_id, street_address, city, postal_code)
VALUES (3, 3, '789 Pine St', 'Sometown', '54321');

-- Insert multiple orders for User 3
INSERT INTO UserOrder (id, user_id, date, amount) VALUES (5, 3, '2024-05-15', 210.45);
INSERT INTO UserOrder (id, user_id, date, amount) VALUES (6, 3, '2024-06-30', 425.80);
