package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.DTO.VerifyUser;
import com.eazybooks.wishlist.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.wishlist.exceptions.BookNotFoundException;
import com.eazybooks.wishlist.exceptions.InternalServerException;
import com.eazybooks.wishlist.exceptions.InvalidUserTokenException;
import com.eazybooks.wishlist.exceptions.UserNotFoundException;
import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.DTO.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.net.UnknownHostException;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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

  @Autowired
  private DiscoveryClient discoveryClient;

  RestTemplate restTemplate = new RestTemplate();


  private Boolean verifyToken (VerifyToken tokenRequest) throws AuthorizationHeaderNotFound {

    if (Objects.isNull(tokenRequest)) {
      logger.warn("Request can not be empty");
      throw new AuthorizationHeaderNotFound("Request can not be empty");
    }
    String authHeader = tokenRequest.getToken();

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      throw new AuthorizationHeaderNotFound("Authorization header missing or invalid");
    }

    String token = tokenRequest.getToken().substring(7);
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
      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, tokenRequest.getUsername().trim()), headers);
      authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("Token validation failed");
        throw new InvalidUserTokenException("Token validation failed");
      }
      return true;
    } catch (Exception e) {
      throw new InvalidUserTokenException("Token validation failed");
    }
  }

  private Boolean verifyUser( VerifyUser verifyUserRequest)
      throws AuthorizationHeaderNotFound {

    String authHeader = verifyUserRequest.getToken();

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      throw new AuthorizationHeaderNotFound("Authorization header missing or invalid");
    }

    String token = verifyUserRequest.getToken().substring(7);
    ResponseEntity<Boolean> authResponse;
    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      logger.info("Found {} instances of authentication service", instances.size());

      ServiceInstance instance = instances.get(0);
      String authUrl = instance.getUri() +"/auth/"+ verifyUserRequest.getUsername()+"/verify-user";

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      logger.info("Service url: " + authUrl);
      HttpEntity<String> requestEntity = new HttpEntity<>(token, headers);

      authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("User verification failed");
        throw new UserNotFoundException("User not found");
      }
      return true;
    } catch (Exception e) {
      throw new UserNotFoundException("User not found");
    }

  }
  public Boolean verifyUserToken (VerifyToken tokenRequest) throws AuthorizationHeaderNotFound {
    return verifyToken(tokenRequest);
  }

  public BookCatalogue verifyBookIsbn(VerifyToken verifyTokenRequest, Long bookIsbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException {

    verifyToken(verifyTokenRequest);

    ResponseEntity<BookCatalogue> authResponse = null;

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances(
          "bookcatalogue");
      if (instances.isEmpty()) {
        logger.error("Book Catalogue not found");
        throw new InternalServerException("Book Catalogue service not found");
      }

      ServiceInstance bookInstance = instances.get(0);
      String bookCatalogueUrl = bookInstance.getUri() + "/bookcatalogue/isbn/" + bookIsbn;

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", verifyTokenRequest.getToken());
      headers.setContentType(MediaType.APPLICATION_JSON);

      HttpEntity<BookCatalogue> requestEntity = new HttpEntity<>(new BookCatalogue(bookIsbn),
          headers);

      authResponse = restTemplate.exchange(
          bookCatalogueUrl, HttpMethod.GET, requestEntity, BookCatalogue.class);

      if (authResponse.getStatusCode() != HttpStatus.OK &&
          !authResponse.getBody().equals(String.valueOf(bookIsbn))) {
        logger.warn("Book not found");
        throw new BookNotFoundException("Book not found");
      }

    } catch (Exception | BookNotFoundException e) {
      throw new BookNotFoundException("Book not found");
    }
    return authResponse.getBody();
  }

    public Boolean verifyUserExists (VerifyUser verifyUserRequest)
      throws AuthorizationHeaderNotFound {
    return  verifyUser(verifyUserRequest);
  }
}

