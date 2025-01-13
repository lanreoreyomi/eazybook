CREATE TABLE IF NOT EXISTS authentication (
                       user_id VARCHAR(255) PRIMARY KEY,
                       username VARCHAR(255) UNIQUE NOT NULL,
                       password VARCHAR(255) NOT NULL,
                       first_name VARCHAR(255),
                       last_name VARCHAR(255),
                       email VARCHAR(255) UNIQUE NOT NULL,
                       role VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS Token (
                       id VARCHAR(255) PRIMARY KEY,
                       username VARCHAR(255)  NOT NULL,
                       token VARCHAR(255)  NOT NULL,
                       is_logged_out BOOLEAN DEFAULT TRUE


);