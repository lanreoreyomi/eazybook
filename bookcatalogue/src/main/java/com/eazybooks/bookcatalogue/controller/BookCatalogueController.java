package com.eazybooks.bookcatalogue.controller;


import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.VerifyToken;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import jakarta.servlet.http.HttpServletRequest;
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
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/bookcatalogue")
public class BookCatalogueController {

  Logger logger = LoggerFactory.getLogger(BookCatalogueController.class);

  private final BookCatalogueService bookCatalogueService;
  private DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();

  public BookCatalogueController(BookCatalogueService bookCatalogueService,
      DiscoveryClient discoveryClient) {
    this.bookCatalogueService = bookCatalogueService;
    this.discoveryClient = discoveryClient;
  }

  @GetMapping()
  public ResponseEntity<List<BookCatalogue>> getAllBookCatalogues(HttpServletRequest request) {

    logger.info("request {}", request.toString());
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String token = authHeader.substring(7);
    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        logger.error("Authentication service not found");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }
      ServiceInstance instance = instances.get(0);

      String authUrl = instance.getUri() + "/auth/validate-token";

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token), headers);
      ResponseEntity<Boolean> authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("Token validation failed");
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();

      }
      List<BookCatalogue> books = bookCatalogueService.getAllCatalogue();
      return ResponseEntity.ok(books);
    } catch (Exception e) {
      logger.error("Error validating user token");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
  }

  @GetMapping("/isbn/{isbn}")
  public ResponseEntity<BookCatalogue> getBookByIsbn(@PathVariable Long isbn, HttpServletRequest request) {

    logger.info("request {}", request.toString());

    try {
      final ResponseEntity<Boolean> verifyTokenResponse = verifyToken(request);

      if (verifyTokenResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          verifyTokenResponse.getBody())) {
        logger.warn("Token validation failed");
        return new ResponseEntity<>(null, HttpStatus.UNAUTHORIZED);
      }
    } catch (Exception e) {
      logger.error("Error validating user token");
      return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
    }
    BookCatalogue bookByIsbn = null;

    try {
      bookByIsbn = bookCatalogueService.getBookByIsbn(isbn);
      logger.info("bookByIsbn {}", bookByIsbn);
    } catch (Exception e) {
      logger.error("Error validating user token");
    }
    if (bookByIsbn ==null) {
      return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
    }
    return new ResponseEntity<>(bookByIsbn, HttpStatus.OK);
  }

  @GetMapping("/book/{bookId}")
  public ResponseEntity<BookCatalogue> getBookCatalogueById(HttpServletRequest request,
      @PathVariable Long bookId) {
    final BookCatalogue bookById = bookCatalogueService.getBookById(bookId);
    logger.info("request {}", bookById.toString());
    return ResponseEntity.ok(bookById);
  }

  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request) {

    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        logger.error("Authentication service not found");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }

      ServiceInstance instance = instances.get(0);
      String authUrl = instance.getUri() + "/auth/validate-token";
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token), headers);
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

}
