package com.eazybooks.bookcatalogue.controller;
import com.eazybooks.bookcatalogue.DTO.VerifyToken;
 import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
 import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
 import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
 import com.eazybooks.bookcatalogue.interfaces.ICheckoutItems;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
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

    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);

    final String response = checkoutItemsService.addBookItemsToCheckout(verifyTokenRequest, bookisbn);

    return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
  }

  @GetMapping("/{username}/all")
  private ResponseEntity<List<BookCatalogue>> getAllCheckout(@PathVariable String username, HttpServletRequest request)
      throws AuthorizationHeaderNotFound {

    //verifies token
    if (Objects.isNull(username) || Objects.isNull(request)) {
      logger.warn("Username is null");
      throw new InvalidUserRequestException("Username is null");
    }
VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);

    final List<BookCatalogue> checkoutItems = checkoutItemsService.checkoutItemsForUser(
        verifyTokenRequest);

    return new ResponseEntity<>(checkoutItems, HttpStatus.OK);

  }

  @PostMapping("{username}/{bookisbn}/remove")
  private ResponseEntity<String> removeBookFromCheckout(@PathVariable String username, @PathVariable long bookisbn, HttpServletRequest request)
      throws AuthorizationHeaderNotFound, BookNotFoundException {

    if (Objects.isNull(username) || Objects.isNull(bookisbn) || Objects.isNull(request)) {
      logger.warn("Username is null");
      throw new InvalidUserRequestException("Username is null");
    }

    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);
    final String response = checkoutItemsService.removeCheckoutItems(verifyTokenRequest, bookisbn);

    return new ResponseEntity<>(response, HttpStatus.OK);

  }


}




