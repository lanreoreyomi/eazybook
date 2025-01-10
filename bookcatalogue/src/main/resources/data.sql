DELETE  FROM book_catalogue where id is not null;

INSERT INTO book_catalogue (id, title, author, isbn, publication_year, description, available, quantity_for_rent) VALUES
                                                                                                                      (gen_random_uuid(), 'The Lord of the Rings', 'J.R.R. Tolkien', '9780618002213', '1954', 'Epic high fantasy trilogy', TRUE, 5),
                                                                                                                      (gen_random_uuid(), 'Pride and Prejudice', 'Jane Austen', '9780141439518', '1813', 'Classic romantic novel', TRUE, 3),
                                                                                                                      (gen_random_uuid(), 'The Hitchhiker''s Guide to the Galaxy', 'Douglas Adams', '9780345391803', '1979', 'Sci-fi comedy series', FALSE, 0),
                                                                                                                      (gen_random_uuid(), 'The Book Thief', 'Markus Zusak', '9780375831003', '2005', 'Historical fiction set in Nazi Germany', TRUE, 2),
                                                                                                                      (gen_random_uuid(), 'To Kill a Mockingbird', 'Harper Lee', '9780061120084', '1960', 'Classic novel about racial injustice', TRUE, 7),
                                                                                                                      (gen_random_uuid(), 'The Great Gatsby', 'F. Scott Fitzgerald', '9780743273565', '1925', 'Tragic love story set in the Jazz Age', TRUE, 4),
                                                                                                                      (gen_random_uuid(), 'Harry Potter and the Philosopher''s Stone', 'J.K. Rowling', '9780747532699', '1997', 'First book in the Harry Potter series', TRUE, 8),
                                                                                                                      (gen_random_uuid(), 'The Hunger Games', 'Suzanne Collins', '9780439023481', '2008', 'Dystopian young adult novel', FALSE, 0),
                                                                                                                      (gen_random_uuid(), 'And Then There Were None', 'Agatha Christie', '9780062073488', '1939', 'Mystery novel', TRUE, 1),
                                                                                                                      (gen_random_uuid(), 'The Da Vinci Code', 'Dan Brown', '9780307474278', '2003', 'Thriller novel', TRUE, 6),
                                                                                                                      (gen_random_uuid(), 'The Girl with the Dragon Tattoo', 'Stieg Larsson', '9780307269763', '2005', 'Crime thriller', TRUE, 3),
                                                                                                                      (gen_random_uuid(), 'The Secret Garden', 'Frances Hodgson Burnett', '9780143106068', '1911', 'Children''s classic', TRUE, 2),
                                                                                                                      (gen_random_uuid(), 'The Catcher in the Rye', 'J.D. Salinger', '9780316769488', '1951', 'Coming-of-age novel', TRUE, 5),
                                                                                                                      (gen_random_uuid(), 'Animal Farm', 'George Orwell', '9780451526342', '1945', 'Satirical novella', TRUE, 4),
                                                                                                                      (gen_random_uuid(), '1984', 'George Orwell', '9780452284234', '1949', 'Dystopian novel', TRUE, 7),
                                                                                                                      (gen_random_uuid(), 'The Hobbit', 'J.R.R. Tolkien', '9780547928227', '1937', 'Fantasy novel', TRUE, 3),
                                                                                                                      (gen_random_uuid(), 'Jane Eyre', 'Charlotte Brontë', '9780142437209', '1847', 'Gothic romance novel', TRUE, 6),
                                                                                                                      (gen_random_uuid(), 'The Little Prince', 'Antoine de Saint-Exupéry', '9780156012195', '1943', 'Philosophical novella', TRUE, 2),
                                                                                                                      (gen_random_uuid(), 'The Alchemist', 'Paulo Coelho', '9780062315007', '1988', 'Inspirational novel', TRUE, 8),
                                                                                                                      (gen_random_uuid(), 'Gone Girl', 'Gillian Flynn', '9780307588364', '2012', 'Psychological thriller', TRUE, 4);