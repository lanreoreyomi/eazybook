package com.eazybooks.bookcatalogue.controller;
import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookExistInCheckoutException;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutItems;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.CheckoutItems;
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

@RestController
@RequestMapping("/checkoutitems/")
 public class CheckoutItemsController {
  Logger logger = LoggerFactory.getLogger(CheckoutItemsController.class);

  private final IBookCatalogue bookCatalogueService;
  private final ICheckoutItems checkoutItemsService;
  private final VerificationService verificationService;

  public CheckoutItemsController(
      IBookCatalogue bookCatalogueService, ICheckoutItems checkoutItemsService,
      VerificationService verificationService) {
     this.bookCatalogueService = bookCatalogueService;
    this.checkoutItemsService = checkoutItemsService;
    this.verificationService = verificationService;
  }
//
//  @PostMapping("{username}/{bookisbn}/add")
//  private ResponseEntity<String> addBookToCheckout(@PathVariable String username, @PathVariable Long bookisbn, HttpServletRequest request)
//      throws AuthorizationHeaderNotFound {
//    logger.info("Adding to book checkout");
//    //verifies token
//    if (Objects.isNull(username)) {
//      logger.warn("Username is null");
//     throw new InvalidUserRequestException("Username is null");
//    }
//
//    try {
//
//      VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);
//      Boolean tokenValid = verificationService.verifyUserToken(verifyToken);
//      if (!Boolean.TRUE.equals(tokenValid)) {
//        logger.error("Error validating token");
//       throw new InvalidUserTokenException("Error validating token");
//      }
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    BookCatalogue bookByIsbn =null;
//    try {
//      bookByIsbn = bookCatalogueService.getBookByIsbn(bookisbn);
//      if (bookByIsbn == null) {
//        throw new BookNotFoundException("Book not found");
//       }
//    } catch (Exception | BookNotFoundException e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    final List<CheckoutItems> checkoutItemsByBookIsbn = checkoutItemsService.findCheckoutItemsByBookIsbn(
//        bookisbn);
//
//    final CheckoutItems alreadyAdded = checkoutItemsByBookIsbn.stream()
//        .filter(item -> item.getBookIsbn().equals(bookisbn) && Objects.equals(item.getUsername(),
//            username))
//        .findFirst()
//        .orElseThrow(()-> new BookExistInCheckoutException("Book already in checkout"));
//
//    try{
//      checkoutItemsService.save(new CheckoutItems(username, bookisbn));
//      return new ResponseEntity<>("Added" + bookByIsbn.getTitle()+ " to checkout", HttpStatus.CREATED);
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());    }
//  }
//
//  @PostMapping("{username}/{bookisbn}/remove")
//  private ResponseEntity<String> removeBookFromCheckout(@PathVariable String username, @PathVariable Long bookisbn, HttpServletRequest request)
//      throws AuthorizationHeaderNotFound {
//    logger.info("Adding to book checkout");
//
//    if (Objects.isNull(username) || Objects.isNull(bookisbn) || Objects.isNull(request)) {
//      logger.warn("Username is null");
//      throw new InvalidUserRequestException("Username is null");
//    }
//
//    try {
//      VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);
//      Boolean tokenValid = verificationService.verifyUserToken(verifyToken);
//      if (!Boolean.TRUE.equals(tokenValid)) {
//        logger.error("Error validating token");
//        throw new InvalidUserTokenException("Error validating token");
//      }
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    BookCatalogue bookByIsbn = null;
//    try {
//      bookByIsbn = bookCatalogueService.getBookByIsbn(bookisbn);
//      if (bookByIsbn == null) {
//       throw new BookNotFoundException("Book not found");
//      }
//    } catch (Exception | BookNotFoundException e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//    final List<CheckoutItems> checkoutItemsByBookIsbn = checkoutItemsService.findCheckoutItemsByBookIsbn(
//        bookisbn);
//
//    final CheckoutItems alreadyAdded = checkoutItemsByBookIsbn.stream()
//        .filter(item -> item.getBookIsbn().equals(bookisbn) && Objects.equals(item.getUsername(),
//            username))
//        .findFirst()
//        .orElseThrow(()-> new BookExistInCheckoutException("Book already in checkout"));// Or provide a default value
//
//    try {
//      checkoutItemsService.deleteCheckoutItemsByBookIsbn(alreadyAdded.getBookIsbn());
//      logger.info("Checkout items  deleted");
//      return new ResponseEntity<>(bookByIsbn.getTitle() +"removed book from checkout", HttpStatus.OK);
//    } catch (Exception e) {
//      logger.error("Something went wrong deleting book", e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//  }
//
//  @GetMapping("/{username}/all")
//  private ResponseEntity<List<BookCatalogue>> getAllCheckout(@PathVariable String username, HttpServletRequest request)
//      throws AuthorizationHeaderNotFound {
//
//    //verifies token
//    if (Objects.isNull(username) || Objects.isNull(request)) {
//      logger.warn("Username is null");
//      throw new InvalidUserRequestException("Username is null");
//    }
//
//    try {
//      VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);
//      Boolean tokenValid = verificationService.verifyUserToken(verifyToken);
//      if (!Boolean.TRUE.equals(tokenValid)) {
//        logger.error("Error validating token");
//        throw new InvalidUserTokenException("Error validating token");
//      }
//    } catch (Exception e) {
//      logger.error(e.getMessage());
//      throw new InternalServerException(e.getMessage());
//    }
//
//     List<CheckoutItems> checkoutItemsByusername = null;
//
//    try {
//      checkoutItemsByusername = checkoutItemsService.findCheckoutItemsByUsername(username);
//
//      if (checkoutItemsByusername == null) {
//        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
//      }
//
//    } catch (Exception e) {
//    logger.warn("Something went wrong getting checkout items");
//    throw  new InternalServerException(e.getMessage());
//    }
//
//    List<BookCatalogue> bookCheckoutItems = new ArrayList<>();
//
//    checkoutItemsByusername.forEach(item -> {
//      if(item.getUsername().equals(username)) {
//        final BookCatalogue bookByIsbn;
//        try {
//          bookByIsbn = bookCatalogueService.getBookByIsbn(item.getBookIsbn());
//        } catch (BookNotFoundException e) {
//          throw new InternalServerException(e.getMessage());
//        }
//        bookCheckoutItems.add(0, bookByIsbn);
//      }
//
//    });
//
//    return new ResponseEntity<>(bookCheckoutItems, HttpStatus.CREATED);
  }


