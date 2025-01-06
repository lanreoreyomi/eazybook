package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import java.util.List;
import java.util.Optional;
 import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface BookCatalogueRepository extends JpaRepository<BookCatalogue, Long> {


  BookCatalogue save(BookCatalogue book);

  List<BookCatalogue> findByPublicationYear(int publicationYear);
  List<BookCatalogue> findByAuthor(String authorName);
  BookCatalogue findByTitle(String title);
  Optional<BookCatalogue> findById(Long id);

  @Query("SELECT i FROM BookCatalogue i WHERE i.isbn = :isbn")
  BookCatalogue findByBookByIsbn(Long isbn);


}
