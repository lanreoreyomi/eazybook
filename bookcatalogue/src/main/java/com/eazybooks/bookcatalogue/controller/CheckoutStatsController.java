package com.eazybooks.bookcatalogue.controller;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.CheckoutStatsService;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/stats/")
public class CheckoutStatsController {
  Logger logger = LoggerFactory.getLogger(CheckoutStatsController.class);

  final CheckoutStatsService checkoutStatsService;

  @Autowired
  RestTemplate standardRestTemplate;
  private final VerificationService verificationService;


  public CheckoutStatsController(CheckoutStatsService checkoutStatsService,
      VerificationService verificationService) {
    this.checkoutStatsService = checkoutStatsService;
    this.verificationService = verificationService;
  }

  @GetMapping("all")
  public ResponseEntity<CheckoutStats> getMaxCheckoutOut(HttpServletRequest request) {

     try {
      ResponseEntity<Boolean> tokenValidation = verificationService.verifyUserToken(request, null);
       final CheckoutStats maxCheckoutStats = checkoutStatsService.getAllCheckoutStats();

       if(tokenValidation.getStatusCode().equals(HttpStatus.OK) && maxCheckoutStats != null) {
         return new ResponseEntity<>(maxCheckoutStats, HttpStatus.OK);
       }

    } catch (Exception e) {
      logger.error("Error Getting Max Checkout Out", e);
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
     return null;
  }
}
