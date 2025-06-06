package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface BookCatalogueRepository extends JpaRepository<BookCatalogue, Long> {
   List<BookCatalogue> findByAuthor(String authorName);
   Optional<BookCatalogue> findById(String id);
  @Query("SELECT i FROM BookCatalogue i WHERE i.isbn = :isbn")
  Optional<BookCatalogue> findByBookByIsbn(Long isbn);
}
