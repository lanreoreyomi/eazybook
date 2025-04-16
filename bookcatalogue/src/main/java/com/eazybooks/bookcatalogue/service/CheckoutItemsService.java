package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.enums.STRINGENUMS;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookExistInCheckoutException;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.exceptions.UserNotFoundException;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutItems;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.CheckoutItems;
import com.eazybooks.bookcatalogue.repository.CheckoutItemsRepository;
import jakarta.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class CheckoutItemsService implements ICheckoutItems {

  Logger logger = LoggerFactory.getLogger(CheckoutItemsService.class);

  CheckoutItemsRepository checkoutItemsRepository;
  private final IBookCatalogue bookCatalogueService;
  private final VerificationService verificationService;

  public CheckoutItemsService(IBookCatalogue bookCatalogueService,
      VerificationService verificationService, CheckoutItemsRepository checkoutItemsRepository) {
    this.bookCatalogueService = bookCatalogueService;
     this.verificationService = verificationService;
     this.checkoutItemsRepository = checkoutItemsRepository;
  }


  @Override
  public CheckoutItems save(CheckoutItems checkoutItems) {
    return checkoutItemsRepository.save(checkoutItems);
  }

  @Override
  public List<CheckoutItems> findCheckoutItemsByBookIsbn(Long bookId) {
    return checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookId).get();
  }

  @Override
  public List<CheckoutItems> findCheckoutItemsByUsername(String username) {
    return checkoutItemsRepository.findCheckoutItemsByUsername(username).get();
  }

  @Override
  public void deleteCheckoutItemsByBookIsbn(Long bookIsbn) {
  checkoutItemsRepository.deleteCheckoutItemsByBookIsbn(bookIsbn);
  }

  @Override
  public String addBookItemsToCheckout(VerifyToken verifyToken, Long bookisbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException {

    if(Objects.isNull(verifyToken) || Objects.isNull(bookisbn)){
      logger.error("Invalid request from user");
      throw new InvalidUserTokenException("Invalid request from user");
    }

VerifyUser verifyUserRequest = new VerifyUser(verifyToken.getToken(), verifyToken.getUsername() );

    if(Objects.isNull(verifyUserRequest)){
      logger.error("Invalid request from user");
      throw new InvalidUserTokenException("Invalid request from user");
    }

    checkIfUserExist(verifyUserRequest);

    validateToken(verifyToken);

    BookCatalogue bookByIsbn =null;
    try {
       bookByIsbn = bookCatalogueService.getBookByIsbn(verifyToken, bookisbn);
      if (Objects.isNull(bookByIsbn)) {
        throw new BookNotFoundException("Book not found");
      }
    } catch (Exception | BookNotFoundException e) {
      logger.error(e.getMessage());
      throw new BookNotFoundException("Book not found");
    }
    System.out.println("bookByIsbn = " + bookByIsbn);
    final List<CheckoutItems> checkoutItemsByBookIsbn = findCheckoutItemsByBookIsbn(
        Long.valueOf(bookisbn));

    final CheckoutItems alreadyAdded = checkoutItemsByBookIsbn.stream()
        .filter(item -> item.getBookIsbn().equals(bookisbn) && Objects.equals(item.getUsername(),
            verifyToken.getUsername()))
        .findFirst().orElse(null);

    if(!Objects.isNull(alreadyAdded)){
      logger.error("Book already in checkout");
      throw new BookExistInCheckoutException("Book already in checkout");
    }

    try{
      checkoutItemsRepository.save(new com.eazybooks.bookcatalogue.model.CheckoutItems(verifyToken.getUsername(), bookisbn));
      return STRINGENUMS.SUCCESS.toString();
    } catch (Exception e) {
      logger.error(e.getMessage());
        throw new InternalServerException("Error adding book to checkout");    }
  }

@Override
public List<BookCatalogue> checkoutItemsForUser(VerifyToken verifyTokenRequest)
    throws AuthorizationHeaderNotFound {

    if(Objects.isNull(verifyTokenRequest)){
      logger.error("Invalid request from user");
      throw new InvalidUserTokenException("Invalid request from user");
    }
    VerifyUser verifyUserRequest = new VerifyUser(verifyTokenRequest.getToken(), verifyTokenRequest.getUsername());

    checkIfUserExist(verifyUserRequest);

    validateToken(verifyTokenRequest);

    List<CheckoutItems> checkoutItemsByusername = null;

    try {
      checkoutItemsByusername = findCheckoutItemsByUsername(verifyTokenRequest.getUsername() );
      if (checkoutItemsByusername == null) {
        return new ArrayList<>();
      }

    } catch (Exception e) {
      logger.warn("Something went wrong getting checkout items for: {}", verifyTokenRequest.getUsername());
      throw new InternalServerException(e.getMessage());
    }

    List<BookCatalogue> bookCheckoutItems = new ArrayList<>();

    checkoutItemsByusername.forEach(item -> {
      final BookCatalogue bookByIsbn;
      if (item.getUsername().equals(verifyTokenRequest.getUsername())) {
        try {
          bookByIsbn = bookCatalogueService.getBookByIsbn(verifyTokenRequest, item.getBookIsbn());
        } catch (BookNotFoundException | AuthorizationHeaderNotFound e) {
          throw new InternalServerException(e.getMessage());
        }
        bookCheckoutItems.add(0, bookByIsbn);
      }

    });
    return bookCheckoutItems;
  }

  @Override
  public String removeCheckoutItems(VerifyToken verifyTokenRequest, Long bookisbn) throws AuthorizationHeaderNotFound, BookNotFoundException {

    if(Objects.isNull(verifyTokenRequest) || Objects.isNull(bookisbn)){
      logger.error("Invalid request from user");
      throw new InvalidUserTokenException("Invalid request from user");
    }

    VerifyUser verifyUserRequest = new VerifyUser(verifyTokenRequest.getToken(), verifyTokenRequest.getUsername());

    checkIfUserExist(verifyUserRequest);

    validateToken(verifyTokenRequest);

    BookCatalogue bookByIsbn = null;
    try {
      bookByIsbn = bookCatalogueService.getBookByIsbn(verifyTokenRequest, bookisbn);
      if (bookByIsbn == null) {
        throw new BookNotFoundException("Book not found");
      }
    } catch (Exception | BookNotFoundException e) {
      logger.error(e.getMessage());
      throw new InternalServerException(e.getMessage());
    }

    final List<CheckoutItems> checkoutItemsByBookIsbn = findCheckoutItemsByBookIsbn(
        bookisbn);

    final CheckoutItems bookInCheckout = checkoutItemsByBookIsbn.stream()
        .filter(item -> item.getBookIsbn().equals(bookisbn) && Objects.equals(item.getUsername(),
            verifyTokenRequest.getUsername()))
        .findFirst()
        .orElseThrow(()-> new BookNotFoundException("Must be checked out to removed from checkout"));// Or provide a default value

      checkoutItemsRepository.deleteCheckoutItemsByBookIsbn(bookInCheckout.getBookIsbn());
      logger.info("Checkout items  deleted");
      return STRINGENUMS.SUCCESS.toString();
  }




  private void checkIfUserExist(VerifyUser verifyUserRequest) throws AuthorizationHeaderNotFound {

    try {
      Boolean userValidation = verificationService.verifyUserExists(verifyUserRequest);
      if (!Boolean.TRUE.equals(userValidation)) {
        logger.error("Error user ");
        throw new UserNotFoundException("User not found");
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      throw new UserNotFoundException("User not found");
    }
  }




//Refactor these common methods
  private void validateToken(VerifyToken verifyToken) throws AuthorizationHeaderNotFound {
    try {
      VerifyToken tokenRequest = new VerifyToken(verifyToken.getToken(), verifyToken.getUsername());
      Boolean tokenValidation = verificationService.verifyUserToken(tokenRequest);
      if (!Boolean.TRUE.equals(tokenValidation)) {
        logger.error("Error validating token");
        throw new InvalidUserTokenException("Error validating user token");
      }
    } catch (Exception e) {
      throw new InvalidUserTokenException("Error validating user token");
    }
  }

}
