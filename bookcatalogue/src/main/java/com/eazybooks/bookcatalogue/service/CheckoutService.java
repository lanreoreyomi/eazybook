package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.DTO.VerifyUserRole;
import com.eazybooks.bookcatalogue.enums.ROLE;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookNotEligibleForCheckoutException;
import com.eazybooks.bookcatalogue.exceptions.BookNotEligibleForReturnException;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.exceptions.UserNotAdminException;
import com.eazybooks.bookcatalogue.exceptions.UserNotFoundException;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutItems;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutStats;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.interfaces.ICheckout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.model.CheckoutItems;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.repository.CheckoutRepository;
import jakarta.transaction.Transactional;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class CheckoutService implements ICheckout {

  Logger logger = LoggerFactory.getLogger(CheckoutItemsService.class);

  private final CheckoutRepository checkoutRepository;
  private final IBookCatalogue bookCatalogueService;
  final ICheckoutStats checkoutStatsService;
  final ICheckoutItems checkoutItemsService;
  private final VerificationService verificationService;

  public CheckoutService(CheckoutRepository checkoutRepository, IBookCatalogue bookCatalogueService,
      ICheckoutStats checkoutStatsService, ICheckoutItems checkoutItemsService, VerificationService verificationService) {
    this.checkoutRepository = checkoutRepository;
    this.bookCatalogueService = bookCatalogueService;
    this.checkoutStatsService = checkoutStatsService;
    this.checkoutItemsService = checkoutItemsService;
    this.verificationService = verificationService;
  }

  @Override
  public Checkout updateCheckout(Checkout checkout) {
    return checkoutRepository.save(checkout);
  }

  @Override
  public String processCheckout(VerifyToken verifyTokenRequest, Long bookIsbn)
      throws BookNotFoundException, AuthorizationHeaderNotFound {

    if (Objects.isNull(verifyTokenRequest) || Objects.isNull(bookIsbn)) {
      logger.error("Request is null");
      throw new InvalidUserRequestException("Request is null");
    }


      CheckoutStats checkoutStats = null;
      BookCatalogue book = null;
      try {
        checkoutStats = checkoutStatsService.findByIsbn(bookIsbn);
        book = bookCatalogueService.getBookByIsbn(verifyTokenRequest, bookIsbn);

        if (book == null) {
          throw new BookNotFoundException("Book not found");
        }

        if (book.getQuantityForRent() == 0) {
          logger.warn("Book quantity not eligible for checkout: " + book.getQuantityForRent());
          throw new BookNotEligibleForReturnException("Book quantity not eligible for checkout");
        }

        if (checkoutStats != null) {
          if (book.getQuantityForRent() <= 0) {
            book.setAvailable(false);
            logger.info("Max Checkchout reached for book " + bookIsbn);
            throw new BookNotEligibleForReturnException("Max Checkchout reached for book ");
          }
        }
      } catch (Exception e) {
        logger.error(e.getMessage());
        throw new InternalServerException("Error processing stats");
      }

      final List<Checkout> checkoutsByUsername = findCheckoutsByCheckedOutBy(
          verifyTokenRequest.getUsername());

      final Checkout alreadyCheckedOut = checkoutsByUsername.stream()
          .filter(item -> item.getCheckedOutBy().equals(verifyTokenRequest.getUsername())
              && Objects.equals(item.getIsbn(),
              bookIsbn))
          .findFirst()
          .orElse(null);

      if (alreadyCheckedOut != null) {
        throw new BookNotEligibleForCheckoutException( "Book already checked out");
      }

      Checkout checkout = new Checkout();
      LocalDate checkoutDate = LocalDate.now();
      LocalDate expectedReturnDate = LocalDate.now().plusWeeks(2);

      int checkout_counter = 1;

      try {
        checkoutStats = checkoutStatsService.findByIsbn(bookIsbn);
        if (checkoutStats != null) {
          logger.info("Checkout stats found for isbn " + checkoutStats.toString());
          final int totalCheckouts = checkoutStats.getTotalCheckout();
          checkout_counter = totalCheckouts + 1;
          checkoutStats.setTotalCheckout(checkout_counter);
          checkoutStats.setTitle(book.getTitle());
        } else {
          logger.info("Checkout stats not found for isbn " + bookIsbn);
          checkoutStats = new CheckoutStats();
          checkoutStats.setTotalCheckout(1);
          checkoutStats.setBookIsbn(bookIsbn);
          checkoutStats.setTitle(book.getTitle());

        }

        checkout.setCheckedOutBy(verifyTokenRequest.getUsername());
        checkout.setDateOfCheckout(checkoutDate);
        checkout.setExpectedReturnDate(expectedReturnDate);
        checkout.setReturned(false);
        checkout.setIsbn(bookIsbn);
        checkoutRepository.save(checkout);
        checkoutStatsService.save(checkoutStats);
        logger.info("Checkout saved for isbn " + checkout_counter);
        book.setQuantityForRent(book.getQuantityForRent() - 1);

        try {
          // updating the available quantity
          bookCatalogueService.updateBook(book);

        } catch (Exception e) {
          logger.error(e.getMessage());
          throw new InternalServerException("Error Updating  book quantity for rent");
        }
         checkoutItemsService.deleteCheckoutItemsByBookIsbn(bookIsbn);

        return "Book successfully checked out";
      } catch (Exception e) {
        throw new InternalServerException("Error Updating  book quantity for rent");
      }
    }


  @Override
  public List<Checkout> findCheckoutsByCheckedOutBy(String username) {
    return checkoutRepository.findCheckoutsByCheckedOutBy(username);
  }

  @Override
  public String handleBookReturns(String username, Long bookIsbn,
      VerifyToken tokenRequest) throws BookNotFoundException, AuthorizationHeaderNotFound {

    if (Objects.isNull(username) || Objects.isNull(bookIsbn) || Objects.isNull(tokenRequest)) {
      logger.error("Username is null");
      throw new InvalidUserRequestException("User request is empty");
    }

    VerifyUser verifyUserRequest = new VerifyUser(tokenRequest.getToken(), tokenRequest.getUsername() );

    checkIfUserExist(verifyUserRequest);

    validateToken(tokenRequest);

    BookCatalogue book = bookCatalogueService.getBookByIsbn(tokenRequest, bookIsbn);
    if (book == null) {
      throw new BookNotFoundException("Book not found");
    }
    //Check if the user that checked out the book is returning it
    final List<Checkout> checkoutsByUsername = findCheckoutsByCheckedOutBy(
        username);

    final Checkout checkedOutBook = checkoutsByUsername.stream()
        .filter(item -> item.getCheckedOutBy().equals(username) && Objects.equals(item.getIsbn(),
            bookIsbn))
        .findFirst()
        .orElse(null);

    logger.info("Checkout found for isbn " + checkedOutBook);

    if (checkedOutBook == null) {
      logger.info("Book mut be checked out to be returned {}", checkedOutBook);
      throw  new BookNotEligibleForReturnException("Book mut be checked out to be returned");
    }

    if (checkedOutBook.getReturned() ==true){
      throw  new BookNotEligibleForReturnException("Book has already been  returned on "+ checkedOutBook.getExpectedReturnDate());
    }

      checkedOutBook.setReturned(true);
      checkedOutBook.setExpectedReturnDate(LocalDate.now());
      updateCheckout(checkedOutBook);

      //update number
      book.setQuantityForRent(book.getQuantityForRent() + 1);
      bookCatalogueService.updateBook(book);
      logger.info("Book successfully checked out {}", checkedOutBook.getExpectedReturnDate());
      return "Book successfully returned";

  }

  @Override
  public List<CheckoutInfo> getCheckoutInfo(VerifyToken verifyToken, String username)
      throws AuthorizationHeaderNotFound {

  if(Objects.isNull(verifyToken) || Objects.isNull(username)){
    logger.error("Request is null");
    throw new InvalidUserRequestException("Request is null");
  }

    VerifyUser verifyUserRequest = new VerifyUser(verifyToken.getToken(), verifyToken.getUsername() );
    checkIfUserExist(verifyUserRequest);

    validateToken(verifyToken);
    List<Checkout> checkoutsByUsername=null;

    try {
      //Check if the user that checked out the book is returning it
      checkoutsByUsername  = findCheckoutsByCheckedOutBy(username);
      if (checkoutsByUsername == null) {
        logger.info("No checkout history found for user " + username);
        return null;
      }

      logger.info("Checkout found for isbn " + checkoutsByUsername.get(0).getIsbn());
    } catch (Exception e) {
      throw  new InternalServerException(e.getMessage());
    }


    List<CheckoutInfo> checkoutInfo = checkoutsByUsername.stream()
        .map(item -> {
          BookCatalogue bookByIsbn = null;
          try {
            bookByIsbn = bookCatalogueService.getBookByIsbn(verifyToken, item.getIsbn());
          } catch (BookNotFoundException | AuthorizationHeaderNotFound e ) {
            throw new RuntimeException("Book not found");
          }
          return new CheckoutInfo(
              bookByIsbn.getTitle(),
              item.getIsbn(),
              item.getDateOfCheckout(),
              item.getReturned(),
              item.getExpectedReturnDate()
          );
        })
        .toList();
    return  checkoutInfo ;
  }

  private void checkIfUserExist(VerifyUser verifyUserRequest) throws AuthorizationHeaderNotFound {
    Boolean userValidation;
    try {
      userValidation = verificationService.verifyUserExists(verifyUserRequest);
      if (!Boolean.TRUE.equals(userValidation)) {
        logger.error("Error user ");
        throw new UserNotFoundException("User not found");
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      throw new UserNotFoundException("User not found");
    }
  }

  private void verifyUserROLE(VerifyToken verifyTokenRequest) {
    try {
      VerifyUserRole verifyUserRole = new VerifyUserRole(verifyTokenRequest.getUsername(),
          verifyTokenRequest.getToken());
      final String userRole = verificationService.verifyUserRole(verifyUserRole);

      if (!Objects.equals(userRole, ROLE.ADMIN.toString())) {
        logger.error("Only admin can add new book");
        throw new UserNotAdminException("Only admin can add new book");
      }

    } catch (Exception e) {
      logger.error(e.getMessage());
      throw new UserNotAdminException("Only admin can add new book");
    }
  }

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
