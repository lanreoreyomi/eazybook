CREATE TABLE wishlists (
          id UUID PRIMARY KEY,
          book_title VARCHAR(255),
          username VARCHAR(255),
          book_isbn INT,
          created_date DATE
);