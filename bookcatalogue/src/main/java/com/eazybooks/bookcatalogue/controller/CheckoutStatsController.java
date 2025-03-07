package com.eazybooks.bookcatalogue.controller;


import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.CheckoutStatsService;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/stats")
@CrossOrigin(origins = "http://localhost:5173")
public class CheckoutStatsController {
  Logger logger = LoggerFactory.getLogger(CheckoutStatsController.class);

  final CheckoutStatsService checkoutStatsService;
  private final VerificationService verificationService;
  private final DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();

  public CheckoutStatsController(CheckoutStatsService checkoutStatsService,
      VerificationService verificationService,
      DiscoveryClient discoveryClient) {
    this.checkoutStatsService = checkoutStatsService;
    this.verificationService = verificationService;
    this.discoveryClient = discoveryClient;
  }

  @GetMapping("/all")
  public ResponseEntity<CheckoutStats> getMaxCheckoutOut(HttpServletRequest request) {
    //verifies token
    try {
      ResponseEntity<Boolean> tokenValid = verificationService.verifyUserToken(request,null);
      if (!Boolean.TRUE.equals(tokenValid.getBody())) {
        logger.error("Error validating token");
        return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(HttpStatus.INTERNAL_SERVER_ERROR);
    }

    final CheckoutStats maxCheckoutStats = checkoutStatsService.getAllCheckoutStats();

    return null;
  }
}