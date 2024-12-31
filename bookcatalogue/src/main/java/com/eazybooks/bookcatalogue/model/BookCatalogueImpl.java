package com.eazybooks.bookcatalogue.model;

import java.util.List;
import org.springframework.stereotype.Repository;

public interface BookCatalogueImpl {

  List<BookCatalogue> getAllCatalogue();
  List<BookCatalogue> getBookByAuthor(String author);
  BookCatalogue addBookToCatalogue(BookCatalogue book);
  BookCatalogue updateBook(BookCatalogue book);
   void deleteBookById(Long id);
   BookCatalogue getBookById(Long id);
 }
