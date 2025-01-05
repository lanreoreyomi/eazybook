CREATE TABLE IF NOT EXISTS book_catalogue (
                       id SERIAL PRIMARY KEY,
                       title VARCHAR(255) NOT NULL,
                       author VARCHAR(255) NOT NULL,
                       isbn bigint UNIQUE,  -- ISBNs should be unique
                       publication_year INT,
                       description TEXT,
                       available BOOLEAN DEFAULT TRUE,
                       quantity_for_rent INT DEFAULT 0,
                       CHECK (quantity_for_rent >= 0)  -- Ensure quantity is not negative
);
create sequence if not exists book_catalogue_seq
    as integer;

alter sequence book_catalogue_seq owner to eazybook_dbuser;

alter sequence book_catalogue_seq owned by book_catalogue.id;