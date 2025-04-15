package com.eazybooks.bookcatalogue.controller;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUserRole;
import com.eazybooks.bookcatalogue.enums.ROLE;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookExistException;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.exceptions.UserNotAdminException;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
 import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/bookcatalogue/")
 public class BookCatalogueController {

  Logger logger = LoggerFactory.getLogger(BookCatalogueController.class);

  private final IBookCatalogue bookCatalogueService;

  public BookCatalogueController(IBookCatalogue bookCatalogueService) {
    this.bookCatalogueService = bookCatalogueService;
    }

  @PostMapping("{username}/addbook")
  public ResponseEntity<String> addBookToCatalogues(@PathVariable String username,
      @RequestBody BookCatalogue book, HttpServletRequest request)
      throws AuthorizationHeaderNotFound, BookExistException {

    if (Objects.isNull(username)) {
      logger.error("Username is null");
      throw new InvalidUserRequestException("Username is null");
    }

    final BookCatalogue addedBook = bookCatalogueService.addBookToCatalogue(new VerifyToken(request.getHeader("Authorization"),
        username), book);
    return new ResponseEntity<>(addedBook.getTitle() + " added successfully", HttpStatus.CREATED);

  }

  @GetMapping
  public ResponseEntity<List<BookCatalogue>> getAllBookCatalogues(HttpServletRequest request)
      throws AuthorizationHeaderNotFound {
    if(Objects.isNull(request)) {
      logger.error("Request is null");
      throw new AuthorizationHeaderNotFound("Request is null");
    }

        VerifyToken tokenRequest = new VerifyToken(request.getHeader("Authorization") );
        List<BookCatalogue> books = bookCatalogueService.getAllCatalogue(tokenRequest);
        return ResponseEntity.ok(books);

  }

  @GetMapping("isbn/{isbn}")
  public ResponseEntity<BookCatalogue> getBookByIsbn(@PathVariable Long isbn,
      HttpServletRequest request) throws BookNotFoundException, AuthorizationHeaderNotFound {


    if(Objects.isNull(request)) {
      logger.error("Request is null");
      throw new InvalidUserRequestException("Request is null");
    }

    BookCatalogue bookByIsbn = bookCatalogueService.getBookByIsbn(
          new VerifyToken(request.getHeader("Authorization")), isbn);

    return new ResponseEntity<>(bookByIsbn, HttpStatus.OK);
  }

}
