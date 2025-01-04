CREATE TABLE wishlists (
          id SERIAL PRIMARY KEY,
          book_title VARCHAR(255),
          username VARCHAR(255),
          isbn INT,
          created_date DATE
);