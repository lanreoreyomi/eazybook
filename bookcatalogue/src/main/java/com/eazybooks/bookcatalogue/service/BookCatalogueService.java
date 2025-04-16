package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.DTO.VerifyUserRole;
import com.eazybooks.bookcatalogue.enums.ROLE;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookExistException;
 import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.EmptyBookRequestException;
 import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.exceptions.UserNotAdminException;
import com.eazybooks.bookcatalogue.exceptions.UserNotFoundException;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
 import com.eazybooks.bookcatalogue.repository.BookCatalogueRepository;
import jakarta.transaction.Transactional;
 import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 import org.springframework.stereotype.Service;

@Service
@Transactional
public class BookCatalogueService implements IBookCatalogue {

  Logger logger = LoggerFactory.getLogger(BookCatalogueService.class);

  final BookCatalogueRepository bookCatalogueRepository;
  final VerificationService verificationService;

  public BookCatalogueService(BookCatalogueRepository bookCatalogueRepository,
      VerificationService verificationService) {
    this.bookCatalogueRepository = bookCatalogueRepository;
    this.verificationService = verificationService;
  }

  @Override
  public List<BookCatalogue> getAllCatalogue(VerifyToken verifyToken) {
    if (Objects.isNull(verifyToken)) {
      logger.error("Verify token is null");
      throw new InvalidUserRequestException("Verify token is null");
    }
    try {

      Boolean tokenValidation = verificationService.verifyUserToken(verifyToken);

       if (!Boolean.TRUE.equals(tokenValidation)) {
        logger.error("Error validating token");
        throw new InvalidUserTokenException("Error validating user token");
      }

    } catch (Exception | AuthorizationHeaderNotFound e) {
      throw new InvalidUserTokenException("Error validating user token");
    }
    return bookCatalogueRepository.findAll();
  }


  @Override
  public BookCatalogue addBookToCatalogue(VerifyToken verifyTokenRequest,
      BookCatalogue book)
      throws AuthorizationHeaderNotFound, BookExistException {


    if (Objects.isNull(book) || Objects.isNull(verifyTokenRequest)) {
      logger.error("Book isbn is null or request is empty");
      throw new InvalidUserRequestException("Book isbn is null or request is empty");

    }

    verificationService.verifyUserToken(verifyTokenRequest);
    //check if user exists or not
    VerifyUser verifyUserRequest = new VerifyUser(verifyTokenRequest.getToken(),
        verifyTokenRequest.getUsername());

    verificationService.verifyUserExists(verifyUserRequest);

    // Check if User is Admin
    verificationService.verifyUserRole(new VerifyUserRole(verifyTokenRequest.getUsername(), verifyTokenRequest.getToken()));

      final boolean present = bookCatalogueRepository.findByBookByIsbn(book.getIsbn()).isPresent();
      if(present){
        throw new BookExistException("Book already exists");
      }

    return bookCatalogueRepository.save(book);
  }

  @Override
  public BookCatalogue getBookByIsbn(VerifyToken verifyTokenRequest, Long isbn)
      throws BookNotFoundException, InvalidUserTokenException, AuthorizationHeaderNotFound {

      if (Objects.isNull(verifyTokenRequest) || Objects.isNull(isbn)) {
        logger.error("Verify token cannot be empty");
        throw new InvalidUserRequestException("Verify token cannot be empty");
      }

    verificationService.verifyUserToken(verifyTokenRequest);

    logger.info("Book Found: {}", isbn);
    return bookCatalogueRepository.findByBookByIsbn(isbn)
        .orElseThrow(() -> new BookNotFoundException("Book with isbn not found"));
  }

  @Override
  public BookCatalogue updateBook(BookCatalogue book) {
    logger.info("Updating book: {}", book);

    if (Objects.isNull(book)) {
      logger.error("Book isbn is null");
      throw new EmptyBookRequestException("Book isbn is null");
    }
    return bookCatalogueRepository.save(book);
  }

}
