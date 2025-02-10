package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.net.InetAddress;
import java.net.UnknownHostException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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

  @Autowired
  RestTemplate standardRestTemplate;

  private final Logger logger = LoggerFactory.getLogger(VerificationService.class);

  @Value("${authentication.service.url}")
  private String authenticationServiceUrl;

  @Value("${bookcatalogue.service.url}")
  private String bookCatalogueServiceUrl;

  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request, String username) {
    String authHeader = request.getHeader("Authorization");

    logger.info("authHeader");
    logger.info(request.getHeader("Authorization"));
    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

    try {
      String authUrl = authenticationServiceUrl();
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      final String s = username != null ? username : null;
      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token,s ), headers);
      authResponse = standardRestTemplate.exchange(
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
  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request) {

    logger.info("request {}", request.toString());
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;
    try {
      String auth_service_url = authenticationServiceUrl();

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, null), headers);
      authResponse = standardRestTemplate.exchange(
          auth_service_url, HttpMethod.POST, requestEntity, Boolean.class);

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

  public ResponseEntity<Boolean> verifyUserToken(HttpServletRequest request, String username) {
    if (username == null) {
      logger.debug("Verifying token without username"); // Add logging
      return verifyToken(request);
    } else {
      logger.debug("Verifying token for username: {}", username); // Add logging
      return verifyToken(request, username);
    }
  }

  private String authenticationServiceUrl() throws UnknownHostException {
    return authenticationServiceUrl+"/auth/validate-token";
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
      String userUrl = bookCatalogueServiceIsbnUrl(bookIsbn);
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", "Bearer " + token);
      headers.setContentType(MediaType.APPLICATION_JSON);

      HttpEntity<BookCatalogue> requestEntity = new HttpEntity<>(new BookCatalogue(bookIsbn),
          headers);

      authResponse = standardRestTemplate.exchange(
          userUrl, HttpMethod.GET, requestEntity, BookCatalogue.class);

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

  private  String bookCatalogueServiceIsbnUrl(Long bookIsbn) {
    return bookCatalogueServiceUrl+"/bookcatalogue/isbn/"+ bookIsbn;
  }
}
