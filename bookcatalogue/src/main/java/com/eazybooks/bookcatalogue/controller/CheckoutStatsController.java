package com.eazybooks.bookcatalogue.controller;


import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutStats;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.checkoutStats;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/stats")
@CrossOrigin(origins = "http://localhost:5173")
public class CheckoutStatsController {
  Logger logger = LoggerFactory.getLogger(CheckoutStatsController.class);

  final ICheckoutStats checkoutStatsService;

  public CheckoutStatsController(ICheckoutStats checkoutStatsService) {
    this.checkoutStatsService = checkoutStatsService;
    }

  @GetMapping("/all")
  public ResponseEntity<CheckoutStats> getMaxCheckoutOut(HttpServletRequest request)
      throws AuthorizationHeaderNotFound {

    if ( Objects.isNull(request)) {
      logger.warn("USer request cannot be empty");
      throw new InvalidUserRequestException("Username cannot be empty");
    }
    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"));

    final CheckoutStats maxCheckoutStats = checkoutStatsService.getAllCheckoutStats(verifyTokenRequest);


    return new ResponseEntity<>(maxCheckoutStats, HttpStatus.OK);
  }
}