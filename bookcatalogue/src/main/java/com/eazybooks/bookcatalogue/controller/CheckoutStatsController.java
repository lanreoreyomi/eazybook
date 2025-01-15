package com.eazybooks.bookcatalogue.controller;

import static com.eazybooks.bookcatalogue.utils.RestUtils.isTokenValid;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.model.MaxCheckoutResponse;
import com.eazybooks.bookcatalogue.service.CheckoutStatsService;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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

  @Autowired
  RestTemplate standardRestTemplate;


  public CheckoutStatsController(CheckoutStatsService checkoutStatsService) {
    this.checkoutStatsService = checkoutStatsService;
  }

  @GetMapping("/all")
  public ResponseEntity<CheckoutStats> getMaxCheckoutOut(HttpServletRequest request) {

     try {
      ResponseEntity<Boolean> tokenValidation = isTokenValid(request, null, logger,
          standardRestTemplate);
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
