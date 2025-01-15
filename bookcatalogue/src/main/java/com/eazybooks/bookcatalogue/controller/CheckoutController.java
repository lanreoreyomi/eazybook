package com.eazybooks.bookcatalogue.controller;

import static com.eazybooks.bookcatalogue.utils.RestUtils.isTokenValid;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.CheckoutItemsService;
import com.eazybooks.bookcatalogue.service.CheckoutService;
import com.eazybooks.bookcatalogue.service.CheckoutStatsService;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/checkout")
@CrossOrigin(origins = "http://localhost:5173")
public class CheckoutController {

  Logger logger = LoggerFactory.getLogger(CheckoutController.class);
  private final CheckoutService checkoutService;
  private final BookCatalogueService bookCatalogueService;
  final CheckoutStatsService checkoutStatsService;
  final CheckoutItemsService checkoutItemsService;

  @Autowired
  RestTemplate standardRestTemplate;

  public CheckoutController(CheckoutService checkoutService,
      BookCatalogueService bookCatalogueService, DiscoveryClient discoveryClient,
      CheckoutStatsService checkoutStatsService, CheckoutItemsService checkoutItemsService) {
    this.checkoutService = checkoutService;
    this.bookCatalogueService = bookCatalogueService;
     this.checkoutStatsService = checkoutStatsService;
    this.checkoutItemsService = checkoutItemsService;
  }

  @PostMapping("/{username}/{bookIsbn}")
  private ResponseEntity<String> checkout(@PathVariable String username,
      @PathVariable Long bookIsbn, HttpServletRequest request) {

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = isTokenValid(request, username, logger, standardRestTemplate);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    CheckoutStats checkoutStats = null;
    BookCatalogue book = null;
    try {
      checkoutStats = checkoutStatsService.findByIsbn(bookIsbn);
      book = bookCatalogueService.getBookByIsbn(bookIsbn);

      if (book == null) {
        return new ResponseEntity<>("Book not found", HttpStatus.NOT_FOUND);
      }

      if (book.getQuantityForRent() == 0) {
        logger.info(
            "Rent not available for isbn because its maxed out " + book.getQuantityForRent());
        return new ResponseEntity<>("Book not available for checkout. Checkout is max out",
            HttpStatus.FORBIDDEN);
      }

      if (book != null && checkoutStats != null) {
        if (book.getQuantityForRent() <= 0) {
          logger.info("Max Checkchout reached for book " + bookIsbn);
          return new ResponseEntity<>("Max Checkchout reached for book ", HttpStatus.FORBIDDEN);
        }
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      throw new RuntimeException(e);
    }

    final List<Checkout> checkoutsByUsername = checkoutService.findCheckoutsByCheckedOutBy(
        username);

    final Checkout alreadyCheckedOut = checkoutsByUsername.stream()
        .filter(item -> item.getCheckedOutBy().equals(username) && Objects.equals(item.getIsbn(),
            bookIsbn))
        .findFirst()
        .orElse(null);// Or provide a default value

    if (alreadyCheckedOut != null) {
      return new ResponseEntity<>("Book already checked out", HttpStatus.FORBIDDEN);
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

      checkout.setCheckedOutBy(username);
      checkout.setDateOfCheckout(checkoutDate);
      checkout.setExpectedReturnDate(expectedReturnDate);
      checkout.setReturned(false);
      checkout.setIsbn(bookIsbn);
      checkoutService.save(checkout);
      checkoutStatsService.save(checkoutStats);
      logger.info("Checkout saved for isbn " + checkout_counter);
      book.setQuantityForRent(book.getQuantityForRent() - 1);
      try {
        // updating the available quantity
        bookCatalogueService.updateBook(book);

      } catch (Exception e) {
        logger.error(e.getMessage());
        return new ResponseEntity<>("Error Updating  book quantity for rent",
            HttpStatus.INTERNAL_SERVER_ERROR);
      }
      //removes book from checkoutItem
      checkoutItemsService.deleteCheckoutItemsByBookIsbn(bookIsbn);

      return new ResponseEntity<>("Book successfully checked out", HttpStatus.CREATED);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }

  }

  @PostMapping("/{username}/{bookIsbn}/return")
  private ResponseEntity<String> returnBook(@PathVariable String username,
      @PathVariable Long bookIsbn, HttpServletRequest request) {

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = isTokenValid(request, username, logger, standardRestTemplate);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    try {
     BookCatalogue book = bookCatalogueService.getBookByIsbn(bookIsbn);
      if (book == null) {
        return new ResponseEntity<>("Book not found", HttpStatus.NOT_FOUND);
      }
      //Check if the user that checked out the book is returning it
      final List<Checkout> checkoutsByUsername = checkoutService.findCheckoutsByCheckedOutBy(
          username);

      final Checkout checkedOutBook = checkoutsByUsername.stream()
          .filter(item -> item.getCheckedOutBy().equals(username) && Objects.equals(item.getIsbn(),
              bookIsbn))
          .findFirst()
          .orElse(null);

      logger.info("Checkout found for isbn " + checkedOutBook);
      if (checkedOutBook == null) {
        logger.info("Book mut be checked out to be returned {}", checkedOutBook);
        return new ResponseEntity<>("Book mut be checked out to be returned", HttpStatus.FORBIDDEN);
      }
      if (checkedOutBook.getReturned()==true){
        return new ResponseEntity<>("Book has already been  returned on "+ checkedOutBook.getExpectedReturnDate(), HttpStatus.FORBIDDEN);
      }
      if (book != null) {
        checkedOutBook.setReturned(true);
        checkedOutBook.setExpectedReturnDate(LocalDate.now());
        checkoutService.updateCheckout(checkedOutBook);

        //update number
        book.setQuantityForRent(book.getQuantityForRent() + 1);
        bookCatalogueService.updateBook(book);
        logger.info("Book successfully checked out {}", checkedOutBook.getExpectedReturnDate());
        return new ResponseEntity<>("Book successfully returned", HttpStatus.OK);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>("Error during Checkout", HttpStatus.INTERNAL_SERVER_ERROR);

    }

    return null;
  }


  @GetMapping("/{username}/all")
  private ResponseEntity<List<CheckoutInfo>> getCheckoutHistory(@PathVariable String username, HttpServletRequest request) {

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = isTokenValid(request, username, logger, standardRestTemplate);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }
     List<Checkout> checkoutsByUsername=null;

    try {
      //Check if the user that checked out the book is returning it
      checkoutsByUsername  = checkoutService.findCheckoutsByCheckedOutBy(username);
      logger.info("Checkout found for isbn " + checkoutsByUsername.get(0).getIsbn());
    } catch (Exception e) {
      logger.info("Checkout not found for username " + username);
      return new ResponseEntity<>( HttpStatus.INTERNAL_SERVER_ERROR);
    }

    if (checkoutsByUsername == null) {
      logger.info("No checkout history found for user " + username);
      return new ResponseEntity<>( HttpStatus.NOT_FOUND);
    }
    List<CheckoutInfo> checkoutInfo = checkoutsByUsername.stream()
        .map(item -> {
          BookCatalogue bookByIsbn = bookCatalogueService.getBookByIsbn(item.getIsbn());
          return new CheckoutInfo(
              bookByIsbn.getTitle(),
              item.getIsbn(),
              item.getDateOfCheckout(),
              item.getReturned(),
              item.getExpectedReturnDate()
              );
        })
        .toList(); // Use toList() in Java 16+ or collect(Collectors.toList())

    return new ResponseEntity<>(checkoutInfo, HttpStatus.OK);

  }

  }
