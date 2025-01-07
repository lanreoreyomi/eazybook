package com.eazybooks.bookcatalogue.controller;

import static com.eazybooks.bookcatalogue.utils.RestUtils.verfiyToken;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.model.SERVICES;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.CheckoutService;
import com.eazybooks.bookcatalogue.service.CheckoutStatsService;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/checkout")
public class CheckoutController {

  Logger logger = LoggerFactory.getLogger(BookCatalogueController.class);
  private final CheckoutService checkoutService;
  private final BookCatalogueService bookCatalogueService;
  CheckoutStatsService checkoutStatsService;
  private final DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();

  public CheckoutController(CheckoutService checkoutService,
      BookCatalogueService bookCatalogueService, DiscoveryClient discoveryClient,
      CheckoutStatsService checkoutStatsService) {
    this.checkoutService = checkoutService;
    this.bookCatalogueService = bookCatalogueService;
    this.discoveryClient = discoveryClient;
    this.checkoutStatsService = checkoutStatsService;
  }

  @PostMapping("/{username}/{bookIsbn}")
  private ResponseEntity<String> checkout(@PathVariable String username,
      @PathVariable Long bookIsbn, HttpServletRequest request) {

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = isTokenValid(request);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }
    //Start here
    //Update available quantity by subtracting checkout
    // from getQuantityForRent and setting the new value in the database

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
      logger.info("alreadyCheckedOut {}", alreadyCheckedOut);
      return new ResponseEntity<>("Book already checked out", HttpStatus.FORBIDDEN);
    }

    Checkout checkout = new Checkout();
    LocalDate checkoutDate = LocalDate.now();

    int checkout_counter = 1;
    try {
      checkoutStats = checkoutStatsService.findByIsbn(bookIsbn);
      logger.info("Checkout found for isbn " + checkout_counter);
      if (checkoutStats != null) {
        final Long totalCheckouts = checkoutStats.getTotalCheckouts();
        checkout_counter = Math.toIntExact(totalCheckouts) + 1;
        checkoutStats.setTotalCheckout((long) checkout_counter);

      } else {
        checkoutStats = new CheckoutStats();
        checkoutStats.setTotalCheckout((long) 1);
        checkoutStats.setBookIsbn(bookIsbn);
      }

      checkout.setCheckedOutBy(username);
      checkout.setDateOfCheckout(checkoutDate);
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
      ResponseEntity<Boolean> tokenValid = isTokenValid(request);
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
        return new ResponseEntity<>("Book has already been  returned on "+ checkedOutBook.getDateOfReturn(), HttpStatus.FORBIDDEN);
      }
      if (book != null) {
        checkedOutBook.setReturned(true);
        checkedOutBook.setDateOfReturn(LocalDate.now());
        checkoutService.updateCheckout(checkedOutBook);

        //update number
        book.setQuantityForRent(book.getQuantityForRent() + 1);
        bookCatalogueService.updateBook(book);
        logger.info("Book successfully checked out {}", checkedOutBook.getDateOfReturn());
        return new ResponseEntity<>("Book successfully checked out", HttpStatus.OK);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>("Error during Checkout", HttpStatus.INTERNAL_SERVER_ERROR);

    }

    return null;
  }



private ResponseEntity<Boolean> isTokenValid(HttpServletRequest request) {

  return verifyToken(request, logger, discoveryClient, restTemplate);

}

private ResponseEntity<Boolean> verifyToken(HttpServletRequest request,
    Logger logger, DiscoveryClient discoveryClient, RestTemplate restTemplate) {
  return verfiyToken(request, logger, discoveryClient, restTemplate);
}


private ResponseEntity<String> verifyUser
    (HttpServletRequest request, String username) {

  String authHeader = request.getHeader("Authorization");

  if (authHeader == null || !authHeader.startsWith("Bearer ")) {
    logger.warn("Authorization header missing or invalid");
    return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
  }

  String token = authHeader.substring(7);
  ResponseEntity<String> authResponse = null;

  try {
    List<ServiceInstance> instances = discoveryClient.getInstances(SERVICES.USER.toString());
    if (instances.isEmpty()) {
      logger.error("User service not found");
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
    }

    ServiceInstance userInstance = instances.get(0);
    String userUrl = userInstance.getUri() + "/user/username/" + username;
    HttpHeaders headers = new HttpHeaders();
    headers.set("Authorization", authHeader);
    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<String> requestEntity = new HttpEntity<>(username, headers);

    authResponse = restTemplate.exchange(
        userUrl, HttpMethod.GET, requestEntity, String.class);

    if (authResponse.getStatusCode() != HttpStatus.OK &&
        !authResponse.getBody().equals(username)) {
      return new ResponseEntity<>("User validation failed", HttpStatus.NOT_FOUND);
    }

    logger.info("User validation successfull");
    return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
  } catch (Exception e) {
    return new ResponseEntity<>("User validation failed", HttpStatus.NOT_FOUND);

  }

}

}
