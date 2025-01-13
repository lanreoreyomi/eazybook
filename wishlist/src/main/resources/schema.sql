CREATE TABLE  IF NOT EXISTS wishlists (
          id VARCHAR(255) PRIMARY KEY,
          book_title VARCHAR(255),
          username VARCHAR(255),
          book_isbn BIGINT,
          created_date DATE
);