package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface BookCatalogueRepository extends JpaRepository<BookCatalogue, Long> {

  BookCatalogue save(BookCatalogue book);
  List<BookCatalogue> findByPublicationYear(String publicationYear);
  List<BookCatalogue> findByAuthor(String authorName);
  BookCatalogue findByTitle(String title);
  BookCatalogue findByBookId(Long bookId);


}
