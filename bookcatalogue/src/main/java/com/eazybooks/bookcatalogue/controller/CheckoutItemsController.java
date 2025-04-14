package com.eazybooks.bookcatalogue.controller;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.CheckoutItems;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.CheckoutItemsService;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
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
@RequestMapping("/checkoutitems/")
 public class CheckoutItemsController {
  Logger logger = LoggerFactory.getLogger(CheckoutItemsController.class);

  private final BookCatalogueService bookCatalogueService;
  private final CheckoutItemsService checkoutItemsService;
  private final VerificationService verificationService;

  public CheckoutItemsController(DiscoveryClient discoveryClient,
      BookCatalogueService bookCatalogueService, CheckoutItemsService checkoutItemsService,
      VerificationService verificationService) {
     this.bookCatalogueService = bookCatalogueService;
    this.checkoutItemsService = checkoutItemsService;
    this.verificationService = verificationService;
  }

  @PostMapping("{username}/{bookisbn}/add")
  private ResponseEntity<String> addBookToCheckout(@PathVariable String username, @PathVariable Long bookisbn, HttpServletRequest request) {
    logger.info("Adding to book checkout");

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = verificationService.verifyUserToken(request, username);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    BookCatalogue bookByIsbn =null;
    try {
      bookByIsbn = bookCatalogueService.getBookByIsbn(bookisbn);
      if (bookByIsbn == null) {
        return new ResponseEntity<>("Book doesnt exist", HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>("Something went wrong", HttpStatus.INTERNAL_SERVER_ERROR);
    }

    final List<CheckoutItems> checkoutItemsByBookIsbn = checkoutItemsService.findCheckoutItemsByBookIsbn(
        bookisbn);

    final CheckoutItems alreadyAdded = checkoutItemsByBookIsbn.stream()
        .filter(item -> item.getBookIsbn().equals(bookisbn) && Objects.equals(item.getUsername(),
            username))
        .findFirst()
        .orElse(null);// Or provide a default value

    if (alreadyAdded != null) {
      return new ResponseEntity<>(bookByIsbn.getTitle()+ " already added to checkout", HttpStatus.FORBIDDEN);
    }

    try{
      checkoutItemsService.save(new CheckoutItems(username, bookisbn));
      return new ResponseEntity<>("Added" + bookByIsbn.getTitle()+ " to checkout", HttpStatus.CREATED);
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>("Something went wrong adding book to checkout", HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @PostMapping("{username}/{bookisbn}/remove")
  private ResponseEntity<String> removeBookToCheckout(@PathVariable String username, @PathVariable Long bookisbn, HttpServletRequest request) {
    logger.info("Adding to book checkout");

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = verificationService.verifyUserToken(request, username);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    BookCatalogue bookByIsbn = null;
    try {
      bookByIsbn = bookCatalogueService.getBookByIsbn(bookisbn);
      if (bookByIsbn == null) {
        return new ResponseEntity<>("Book doesnt exist", HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>("Something went wrong", HttpStatus.INTERNAL_SERVER_ERROR);
    }

    final List<CheckoutItems> checkoutItemsByBookIsbn = checkoutItemsService.findCheckoutItemsByBookIsbn(
        bookisbn);

    final CheckoutItems alreadyAdded = checkoutItemsByBookIsbn.stream()
        .filter(item -> item.getBookIsbn().equals(bookisbn) && Objects.equals(item.getUsername(),
            username))
        .findFirst()
        .orElse(null);// Or provide a default value

    if (alreadyAdded == null) {
    return new ResponseEntity<>(bookByIsbn.getTitle() + "not in checkout", HttpStatus.FORBIDDEN);
    }
    try {
      checkoutItemsService.deleteCheckoutItemsByBookIsbn(alreadyAdded.getBookIsbn());
      logger.info("Checkout items  deleted");
      return new ResponseEntity<>(bookByIsbn.getTitle() +"removed book from checkout", HttpStatus.OK);
    } catch (Exception e) {
      logger.error("Something went wrong deleting book", e.getMessage());
      return new ResponseEntity<>("Something went wrong deleting book",
          HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  @GetMapping("/{username}/all")
  private ResponseEntity<List<BookCatalogue>> getAllCheckout(@PathVariable String username, HttpServletRequest request) {

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = verificationService.verifyUserToken(request, username);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

     List<CheckoutItems> checkoutItemsByusername = null;

    try {
      checkoutItemsByusername = checkoutItemsService.findCheckoutItemsByUsername(username);

      if (checkoutItemsByusername == null) {
        return new ResponseEntity<>(HttpStatus.NOT_FOUND);
      }

    } catch (Exception e) {
    logger.info("Checkout is empty for username: " + username);
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    List<BookCatalogue> bookCheckoutItems = new ArrayList<>();

    checkoutItemsByusername.forEach(item -> {
      if(item.getUsername().equals(username)) {
        final BookCatalogue bookByIsbn = bookCatalogueService.getBookByIsbn(item.getBookIsbn());
        bookCheckoutItems.add(0, bookByIsbn);
      }

    });

    return new ResponseEntity<>(bookCheckoutItems, HttpStatus.CREATED);
  }


}
