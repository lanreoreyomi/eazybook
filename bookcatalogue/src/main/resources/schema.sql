CREATE TABLE IF NOT EXISTS book_catalogue (
                       book_id SERIAL PRIMARY KEY,
                       title VARCHAR(255) NOT NULL,
                       author VARCHAR(255) NOT NULL,
                       isbn VARCHAR(20) UNIQUE,  -- ISBNs should be unique
                       publication_year INT,
                       description TEXT,
                       available BOOLEAN DEFAULT TRUE,
                       quantity_for_rent INT DEFAULT 0,
                       CHECK (quantity_for_rent >= 0)  -- Ensure quantity is not negative
);