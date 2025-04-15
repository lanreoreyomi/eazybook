package com.eazybooks.bookcatalogue.controller;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookNotEligibleForRentException;
import com.eazybooks.bookcatalogue.exceptions.BookNotEligibleForReturnException;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.ICheckout;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.ICheckoutItems;
import com.eazybooks.bookcatalogue.service.IcheckoutStats;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import org.mozilla.javascript.EcmaError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/checkout")
public class CheckoutController {

  Logger logger = LoggerFactory.getLogger(CheckoutController.class);

  private final ICheckout checkoutService;
  private final IBookCatalogue bookCatalogueService;
  final IcheckoutStats checkoutStatsService;
  final ICheckoutItems checkoutItemsService;
  private final DiscoveryClient discoveryClient;
  private final VerificationService verificationService;

  public CheckoutController(ICheckout checkoutService,
      IBookCatalogue bookCatalogueService, DiscoveryClient discoveryClient,
      IcheckoutStats checkoutStatsService, ICheckoutItems checkoutItemsService,
      VerificationService verificationService) {
    this.checkoutService = checkoutService;
    this.bookCatalogueService = bookCatalogueService;
    this.discoveryClient = discoveryClient;
    this.checkoutStatsService = checkoutStatsService;
    this.checkoutItemsService = checkoutItemsService;
    this.verificationService = verificationService;
  }

//  @PostMapping("/{username}/{bookIsbn}")
//   ResponseEntity<String> checkout(@PathVariable String username,
//      @PathVariable Long bookIsbn, HttpServletRequest request) throws AuthorizationHeaderNotFound {
//
//    //verifies token
//    if (Objects.isNull(username)) {
//      logger.warn("Username is null");
//      throw new InvalidUserRequestException("Username is null");
//    }
//
////verifies token
//    try {
//      VerifyToken tokenRequest = new VerifyToken(request.getHeader("Authorization"), username);
//      Boolean tokenValid = verificationService.verifyUserToken(tokenRequest);
//
//      if (!Boolean.TRUE.equals(tokenValid)) {
//        logger.error("Error validating token");
//        throw new InvalidUserTokenException("Error validating token");
//      }
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    CheckoutStats checkoutStats = null;
//    BookCatalogue book = null;
//    try {
//      checkoutStats = checkoutStatsService.findByIsbn(bookIsbn);
//      book = bookCatalogueService.getBookByIsbn(bookIsbn);
//
//      if (book == null) {
//        throw new BookNotFoundException("Book not found");
//      }
//
//      if (book.getQuantityForRent() == 0) {
//        logger.info(
//            "Rent not available for isbn because its maxed out " + book.getQuantityForRent());
//        throw new BookNotEligibleForRentException(
//            "Book not available for checkout. Checkout is max out");
//
//      }
//
//      if (checkoutStats != null) {
//        if (book.getQuantityForRent() <= 0) {
//          book.setAvailable(false);
//          logger.info("Max Checkchout reached for book " + bookIsbn);
//          throw new BookNotEligibleForRentException("Max Checkchout reached for book ");
//        }
//      }
//    } catch (Exception | BookNotFoundException e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    final List<Checkout> checkoutsByUsername = checkoutService.findCheckoutsByCheckedOutBy(
//        username);
//
//    final Checkout alreadyCheckedOut = checkoutsByUsername.stream()
//        .filter(item -> item.getCheckedOutBy().equals(username) && Objects.equals(item.getIsbn(),
//            bookIsbn))
//        .findFirst()
//        .orElse(null);// Or provide a default value
//
//    if (alreadyCheckedOut != null) {
//      throw new BookNotEligibleForRentException("Book already checked out.");
//    }
//
//    Checkout checkout = new Checkout();
//    LocalDate checkoutDate = LocalDate.now();
//    LocalDate expectedReturnDate = LocalDate.now().plusWeeks(2);
//
//    int checkout_counter = 1;
//
//    try {
//      checkoutStats = checkoutStatsService.findByIsbn(bookIsbn);
//      if (checkoutStats != null) {
//        logger.info("Checkout stats found for isbn " + checkoutStats.toString());
//        final int totalCheckouts = checkoutStats.getTotalCheckout();
//        checkout_counter = totalCheckouts + 1;
//        checkoutStats.setTotalCheckout(checkout_counter);
//        checkoutStats.setTitle(book.getTitle());
//      } else {
//        logger.info("Checkout stats not found for isbn " + bookIsbn);
//        checkoutStats = new CheckoutStats();
//        checkoutStats.setTotalCheckout(1);
//        checkoutStats.setBookIsbn(bookIsbn);
//        checkoutStats.setTitle(book.getTitle());
//
//      }
//
//      checkout.setCheckedOutBy(username);
//      checkout.setDateOfCheckout(checkoutDate);
//      checkout.setExpectedReturnDate(expectedReturnDate);
//      checkout.setReturned(false);
//      checkout.setIsbn(bookIsbn);
//      checkoutService.save(checkout);
//      checkoutStatsService.save(checkoutStats);
//      logger.info("Checkout saved for isbn " + checkout_counter);
//      book.setQuantityForRent(book.getQuantityForRent() - 1);
//
//      // updating the available quantity
//      bookCatalogueService.updateBook(book);
//      checkoutItemsService.deleteCheckoutItemsByBookIsbn(bookIsbn);
//
//      return new ResponseEntity<>("Book successfully checked out", HttpStatus.CREATED);
//    } catch (Exception e) {
//      throw new RuntimeException(e);
//    }
//  }
//
//  @PostMapping("/{username}/{bookIsbn}/return")
//  ResponseEntity<String> returnBook(@PathVariable String username,
//      @PathVariable Long bookIsbn, HttpServletRequest request) throws BookNotFoundException {
//
//    if (Objects.isNull(username)) {
//      logger.warn("Username is null");
//      throw new InvalidUserRequestException("Username is null");
//    }
//      try {
//        VerifyToken tokenRequest = new VerifyToken(request.getHeader("Authorization"), username);
//        Boolean tokenValid = verificationService.verifyUserToken(tokenRequest);
//
//        if (!Boolean.TRUE.equals(tokenValid)) {
//          logger.error("Error validating token");
//          throw new InvalidUserTokenException("Error validating token");
//        }
//      } catch (Exception | AuthorizationHeaderNotFound e) {
//        logger.error(e.getMessage());
//    throw new InternalServerException(e.getMessage());
//      }
//
//    try {
//     BookCatalogue book = bookCatalogueService.getBookByIsbn(bookIsbn);
//      if (book == null) {
//      throw new BookNotFoundException("Book not found");
//      }
//      //Check if the user that checked out the book is returning it
//      final List<Checkout> checkoutsByUsername = checkoutService.findCheckoutsByCheckedOutBy(
//          username);
//
//      final Checkout checkedOutBook = checkoutsByUsername.stream()
//          .filter(item -> item.getCheckedOutBy().equals(username) && Objects.equals(item.getIsbn(),
//              bookIsbn))
//          .findFirst()
//          .orElse(null);
//
//      logger.info("Checkout found for isbn " + checkedOutBook);
//      if (checkedOutBook == null) {
//        logger.info("Book mut be checked out to be returned {}", checkedOutBook);
//        throw  new BookNotEligibleForReturnException("Book mut be checked out to be returned");
//       }
//
//      if (checkedOutBook.getReturned()==true){
//        throw  new BookNotEligibleForReturnException("Book has already been  returned on "+ checkedOutBook.getExpectedReturnDate());
//
//       }
//      if (book != null) {
//        checkedOutBook.setReturned(true);
//        checkedOutBook.setExpectedReturnDate(LocalDate.now());
//        checkoutService.updateCheckout(checkedOutBook);
//
//        //update number
//        book.setQuantityForRent(book.getQuantityForRent() + 1);
//        bookCatalogueService.updateBook(book);
//        logger.info("Book successfully checked out {}", checkedOutBook.getExpectedReturnDate());
//        return new ResponseEntity<>("Book successfully returned", HttpStatus.OK);
//      }
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    return null;
//  }
//
//
//  @GetMapping("/{username}/all")
//  ResponseEntity<List<CheckoutInfo>> getCheckoutHistory(@PathVariable String username,
//      HttpServletRequest request) throws AuthorizationHeaderNotFound {
//
//    if(Objects.isNull(request) || Objects.isNull(username)){
//      logger.error("Request is null");
//      throw new InvalidUserRequestException("Request is null");
//    }
//
//    VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);
//    //verifies token
//    try {
//      Boolean tokenValid = verificationService.verifyUserToken(verifyToken);
//      if (!Boolean.TRUE.equals(tokenValid)) {
//        logger.error("Error validating token");
//       throw new InvalidUserTokenException("Error validating token");
//      }
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//     List<Checkout> checkoutsByUsername=null;
//
//    try {
//      //Check if the user that checked out the book is returning it
//      checkoutsByUsername  = checkoutService.findCheckoutsByCheckedOutBy(username);
//      logger.info("Checkout found for isbn " + checkoutsByUsername.get(0).getIsbn());
//    } catch (Exception e) {
//     throw  new InternalServerException(e.getMessage());
//    }
//
//    if (checkoutsByUsername == null) {
//      logger.info("No checkout history found for user " + username);
//      return new ResponseEntity<>( HttpStatus.NO_CONTENT);
//    }
//    List<CheckoutInfo> checkoutInfo = checkoutsByUsername.stream()
//        .map(item -> {
//          BookCatalogue bookByIsbn = null;
//          try {
//            bookByIsbn = bookCatalogueService.getBookByIsbn(item.getIsbn());
//          } catch (BookNotFoundException e) {
//            throw new InternalServerException(e.getMessage());
//          }
//          return new CheckoutInfo(
//              bookByIsbn.getTitle(),
//              item.getIsbn(),
//              item.getDateOfCheckout(),
//              item.getReturned(),
//              item.getExpectedReturnDate()
//              );
//        })
//        .toList(); // Use toList() in Java 16+ or collect(Collectors.toList())
//
//    return new ResponseEntity<>(checkoutInfo, HttpStatus.OK);
//
//  }

  }
