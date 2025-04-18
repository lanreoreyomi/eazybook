package com.eazybooks.bookcatalogue.controller;


import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
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

  final checkoutStats checkoutStatsService;
  private final VerificationService verificationService;

  public CheckoutStatsController(checkoutStats checkoutStatsService,
      VerificationService verificationService) {
    this.checkoutStatsService = checkoutStatsService;
    this.verificationService = verificationService;
   }

  @GetMapping("/all")
  public ResponseEntity<CheckoutStats> getMaxCheckoutOut(HttpServletRequest request)
      throws AuthorizationHeaderNotFound {

    if ( Objects.isNull(request)) {
      logger.warn("Username cannot be empty");
      throw new InvalidUserRequestException("Username cannot be empty");
    }

    try {
      VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"));
      Boolean tokenValid = verificationService.verifyUserToken(verifyTokenRequest);
      if (!Boolean.TRUE.equals(tokenValid)) {
        logger.error("Error validating token");
        throw new InvalidUserTokenException("Error validating token");
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      throw new InternalServerException(e.getMessage());
    }

    final CheckoutStats maxCheckoutStats = checkoutStatsService.getAllCheckoutStats();

    return new ResponseEntity<>(maxCheckoutStats, HttpStatus.OK);
  }
}