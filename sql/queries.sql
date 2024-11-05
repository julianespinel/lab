-- Query to fetch a user with profile, address, and payment info
SELECT u.username, u.email, p.bio, a.street_address, a.city, a.postal_code, pi.card_number
FROM User u
         LEFT JOIN Profile p ON u.id = p.user_id
         LEFT JOIN Address a ON u.id = a.user_id
         LEFT JOIN PaymentInfo pi ON u.id = pi.user_id
WHERE u.id = 1;

-- Query to fetch all orders for a specific user
SELECT o.id, o.order_date, o.amount
FROM UserOrder o
WHERE o.user_id = 1;

-- Get all information available from all users
SELECT *
FROM
    User u
        LEFT JOIN
    Profile p ON u.id = p.user_id
        LEFT JOIN
    Address a ON u.id = a.user_id
        LEFT JOIN
    PaymentInfo pi ON u.id = pi.user_id
        LEFT JOIN
    UserOrder o ON u.id = o.user_id
ORDER BY
    u.id, o.id;
