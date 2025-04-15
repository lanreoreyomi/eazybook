package com.eazybooks.bookcatalogue.interfaces;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookExistException;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import java.util.List;

public interface IBookCatalogue {

  List<BookCatalogue> getAllCatalogue(VerifyToken verifyToken);
   BookCatalogue addBookToCatalogue(VerifyToken verifyTokenRequest, BookCatalogue book)
       throws AuthorizationHeaderNotFound, BookExistException;
  BookCatalogue updateBook(BookCatalogue book);
  BookCatalogue getBookByIsbn(VerifyToken verifyToken, Long isbn)
      throws BookNotFoundException, AuthorizationHeaderNotFound;
 }
