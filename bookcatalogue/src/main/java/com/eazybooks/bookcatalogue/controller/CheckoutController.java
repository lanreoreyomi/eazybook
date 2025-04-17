package com.eazybooks.bookcatalogue.controller;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.interfaces.ICheckout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
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
@RequestMapping("/checkout")
public class CheckoutController {

  Logger logger = LoggerFactory.getLogger(CheckoutController.class);

  private final ICheckout checkoutService;

  public CheckoutController(ICheckout checkoutService) {
    this.checkoutService = checkoutService;
   }

  @PostMapping("/{username}/{bookIsbn}")
  ResponseEntity<String> checkout(@PathVariable String username,
      @PathVariable Long bookIsbn, HttpServletRequest request)
      throws BookNotFoundException, AuthorizationHeaderNotFound {

    if (Objects.isNull(username)) {
      logger.warn("Username can not be empty");
      throw new InvalidUserRequestException("Username can not be emptyl");
    }
    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);

    final String response = checkoutService.processCheckout(verifyTokenRequest, bookIsbn);
    return new ResponseEntity<>(response, HttpStatus.OK);

  }


  @PostMapping("/{username}/{bookIsbn}/return")
  ResponseEntity<String> returnBook(@PathVariable String username,
      @PathVariable Long bookIsbn, HttpServletRequest request)
      throws BookNotFoundException, AuthorizationHeaderNotFound {

    if (Objects.isNull(username)) {
      logger.warn("Username can not be empty");
      throw new InvalidUserRequestException("Username can not be emptyl");
    }

    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);
    final String response = checkoutService.handleBookReturns(username, bookIsbn, verifyTokenRequest);
    return  new ResponseEntity<>(response, HttpStatus.OK) ;
  }

  @GetMapping("/{username}/all")
  ResponseEntity<List<CheckoutInfo>> getCheckoutHistory(@PathVariable String username,
      HttpServletRequest request) throws AuthorizationHeaderNotFound {

    if(Objects.isNull(request) || Objects.isNull(username)){
      logger.error("Request can not be empty");
      throw new InvalidUserRequestException("Request can not be empty");
    }

    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);

    List<CheckoutInfo> checkoutInfo = checkoutService.getCheckoutInfo(verifyTokenRequest, username);

    return new ResponseEntity<>(checkoutInfo, HttpStatus.OK);

  }

  }
