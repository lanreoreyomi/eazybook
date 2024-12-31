CREATE TABLE wishlists (
          id SERIAL PRIMARY KEY,
          user_id INT,
          book_catalogue_id INT,
          book_title VARCHAR(255),
          book_isbn INT,
          created_date DATE
);