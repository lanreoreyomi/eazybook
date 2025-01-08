CREATE TABLE IF NOT EXISTS book_catalogue (
                       id SERIAL PRIMARY KEY,
                       title VARCHAR(255) NOT NULL,
                       author VARCHAR(255) NOT NULL,
                       isbn bigint UNIQUE,  -- ISBNs should be unique
                       publication_year INT,
                       description TEXT,
                       available BOOLEAN DEFAULT TRUE,
                       quantity_for_rent INT DEFAULT 0
                       CHECK (quantity_for_rent >= 0)  -- Ensure quantity is not negative
);
create sequence if not exists book_catalogue_seq as integer;

alter sequence book_catalogue_seq owner to eazybook_dbuser;

alter sequence book_catalogue_seq owned by book_catalogue.id;

CREATE TABLE IF NOT EXISTS checkout (
                         id SERIAL PRIMARY KEY,
                         isbn bigint,
                         checked_out_by VARCHAR(255) NOT NULL,
                         date_of_checkout DATE NOT NULL ,
                         date_of_return DATE ,
                         is_returned BOOLEAN DEFAULT FALSE
 );

create sequence if not exists checkout_seq
    as integer;

alter sequence checkout_seq owner to eazybook_dbuser;

alter sequence checkout_seq owned by checkout.id;

CREATE TABLE IF NOT EXISTS checkout_stats (
                                id SERIAL PRIMARY KEY,
                                total_checkout BIGINT NOT NULL,
                                book_isbn BIGINT UNIQUE NOT NULL

);

CREATE SEQUENCE IF NOT EXISTS checkout_stats_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
 alter sequence checkout_stats_seq owner to eazybook_dbuser;
alter sequence checkout_stats_seq owned by checkout_stats.id;

CREATE TABLE IF NOT EXISTS checkout_items (
                                              id SERIAL PRIMARY KEY,
                                              book_isbn BIGINT NOT NULL,
                                              username VARCHAR(255) NOT NULL


);

CREATE SEQUENCE IF NOT EXISTS checkout_items_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
alter sequence checkout_items_seq owner to eazybook_dbuser;
alter sequence checkout_items_seq owned by checkout_items.id;
