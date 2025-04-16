package com.eazybooks.bookcatalogue.controller;
import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.enums.STRINGENUMS;
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

  private final ICheckoutItems checkoutItemsService;

  public CheckoutItemsController(ICheckoutItems checkoutItemsService) {
    this.checkoutItemsService = checkoutItemsService;

  }

  @PostMapping("{username}/{bookisbn}/add")
  private ResponseEntity<String> addBookToCheckout(@PathVariable String username, @PathVariable Long bookisbn, HttpServletRequest request)
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    logger.info("Adding to book checkout");
    //verifies token
    if (Objects.isNull(username)) {
      logger.warn("Username is null");
     throw new InvalidUserRequestException("Username is null");
    }

    VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);

    final String response = checkoutItemsService.addBookItemsToCheckout(verifyToken, bookisbn);

    if(response.equals(STRINGENUMS.SUCCESS.toString())){
      return new ResponseEntity<>(response, HttpStatus.OK);
        }
    return new ResponseEntity<>("Error adding book to checkout", HttpStatus.INTERNAL_SERVER_ERROR);
  }

  @GetMapping("/{username}/all")
  private ResponseEntity<List<BookCatalogue>> getAllCheckout(@PathVariable String username, HttpServletRequest request)
      throws AuthorizationHeaderNotFound {

    //verifies token
    if (Objects.isNull(username) || Objects.isNull(request)) {
      logger.warn("Username is null");
      throw new InvalidUserRequestException("Username is null");
    }
VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);

    final List<BookCatalogue> checkoutItems = checkoutItemsService.checkoutItemsForUser(
        verifyToken);

    return new ResponseEntity<>(checkoutItems, HttpStatus.OK);

  }

  @PostMapping("{username}/{bookisbn}/remove")
  private ResponseEntity<String> removeBookFromCheckout(@PathVariable String username, @PathVariable Long bookisbn, HttpServletRequest request)
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    logger.info("Adding to book checkout");

    if (Objects.isNull(username) || Objects.isNull(bookisbn) || Objects.isNull(request)) {
      logger.warn("Username is null");
      throw new InvalidUserRequestException("Username is null");
    }

    VerifyToken verifyToken = new VerifyToken(request.getHeader("Authorization"), username);
    final String response = checkoutItemsService.removeCheckoutItems(verifyToken, bookisbn);

    return new ResponseEntity<>(response, HttpStatus.OK);

  }


}




