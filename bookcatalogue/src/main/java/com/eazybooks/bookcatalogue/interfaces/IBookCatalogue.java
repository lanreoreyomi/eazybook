package com.eazybooks.bookcatalogue.interfaces;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import java.util.List;
import org.springframework.data.domain.Pageable;

public interface IBookCatalogue {

  List<BookCatalogue> getAllCatalogue(Pageable pageable);
  List<BookCatalogue> getBookByAuthor(String author);
  BookCatalogue addBookToCatalogue(BookCatalogue book);
  BookCatalogue updateBook(BookCatalogue book);
  BookCatalogue getBookById(Long id);
  BookCatalogue getBookByIsbn(Long isbn);
 }
