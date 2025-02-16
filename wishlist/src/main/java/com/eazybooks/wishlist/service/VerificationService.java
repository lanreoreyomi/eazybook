package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class VerificationService {
  private final Logger logger = LoggerFactory.getLogger(VerificationService.class);

  RestTemplate restTemplate = new RestTemplate();
   @Autowired
   private DiscoveryClient discoveryClient;



  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request, String username) {
    String authHeader = request.getHeader("Authorization");

    logger.info(request.getHeader("Authorization"));
    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      logger.info("Found {} instances of authentication service", instances.size());

      if (instances.isEmpty()) {
        logger.error("Authentication service not found");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }

      ServiceInstance instance = instances.get(0);
      String authUrl = instance.getUri() + "/auth/validate-token";
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      logger.info("Service url: " + authUrl);
      final String s = username != null ? username : null;
      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, s), headers);
      authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("Token validation failed");
        return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    }
    catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>(false, HttpStatus.INTERNAL_SERVER_ERROR);
    }

  }
  private ResponseEntity<Boolean> verifyToken (HttpServletRequest request)
      throws UnknownHostException {

    logger.info("request {}", request.toString());
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      logger.info("Found {} instances of authentication service", instances.size());

      ServiceInstance instance = instances.get(0);
      String authUrl = instance.getUri() + "/auth/validate-token";
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      logger.info("Service url: " + authUrl);
      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, null), headers);
      authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("Token validation failed");
        return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
    }
  }

  public ResponseEntity<Boolean> verifyUserToken (HttpServletRequest request, String username){
    if (username == null) {
      logger.debug("Verifying token without username"); // Add logging
      try {
        return verifyToken(request);
      } catch (UnknownHostException e) {
        throw new RuntimeException(e);
      }
    } else {
      logger.debug("Verifying token for username: {}", username); // Add logging
      return verifyToken(request, username);
    }
  }

  public ResponseEntity<BookCatalogue> verifyBookIsbn( HttpServletRequest request, Long bookIsbn) {

    String authHeader = request.getHeader("Authorization");
    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }
    String token = authHeader.substring(7);

    ResponseEntity<BookCatalogue> authResponse = null;

      try {
        List<ServiceInstance> instances = discoveryClient.getInstances(
            "bookcatalogue");
        if (instances.isEmpty()) {
          logger.error("Book Catalogue not found");
          return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }

        ServiceInstance bookInstance = instances.get(0);
        String bookCatalogueUrl = bookInstance.getUri() + "/bookcatalogue/isbn/" + bookIsbn;

        HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", "Bearer " + token);
      headers.setContentType(MediaType.APPLICATION_JSON);

      HttpEntity<BookCatalogue> requestEntity = new HttpEntity<>(new BookCatalogue(bookIsbn),
          headers);

      authResponse = restTemplate.exchange(
          bookCatalogueUrl, HttpMethod.GET, requestEntity, BookCatalogue.class);

      if (authResponse.getStatusCode() != HttpStatus.OK &&
          !authResponse.getBody().equals(String.valueOf(bookIsbn))) {
        return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
      }

      logger.info("Book found for Isbn");
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);

    }

  }

}
