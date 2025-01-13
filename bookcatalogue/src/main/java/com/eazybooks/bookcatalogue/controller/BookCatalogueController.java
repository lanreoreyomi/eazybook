package com.eazybooks.bookcatalogue.controller;


import static com.eazybooks.bookcatalogue.utils.RestUtils.isTokenValid;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.VerifyToken;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.utils.RestUtils;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/bookcatalogue")
public class BookCatalogueController {

  Logger logger = LoggerFactory.getLogger(BookCatalogueController.class);

  private final BookCatalogueService bookCatalogueService;
  private final DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();

  public BookCatalogueController(BookCatalogueService bookCatalogueService,
      DiscoveryClient discoveryClient) {
    this.bookCatalogueService = bookCatalogueService;
    this.discoveryClient = discoveryClient;
  }

  @PostMapping("/{username}/addbook")
  public ResponseEntity<String> addBookToCatalogues(@PathVariable String username,
      @RequestBody BookCatalogue book, HttpServletRequest request) {

    ResponseEntity<Boolean> tokenValidation = null;

    //verifies token
    try {
      tokenValidation = isTokenValid(request, username, logger, discoveryClient, restTemplate);
    } catch (Exception e) {
      logger.error(e.getMessage());
    }

    assert tokenValidation != null;
    if (!Boolean.TRUE.equals(tokenValidation.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
    }
    // Check if User is Admin
    try {
      final ResponseEntity<String> userRole = getUserRole(username, request);
      if (!Objects.equals(userRole.getBody(), "ADMIN")) {
        return new ResponseEntity<>("Only admin can add new book", HttpStatus.FORBIDDEN);
      }
    } catch (Exception e) {
      return new ResponseEntity<>("Error getting user role", HttpStatus.INTERNAL_SERVER_ERROR);
    }
    try {
      BookCatalogue bookByIsbn = bookCatalogueService.getBookByIsbn(book.getIsbn());
      if (bookByIsbn != null) {
        logger.info(bookByIsbn.toString());
        return new ResponseEntity<>("Book already exist", HttpStatus.CONFLICT);

      }
    } catch (Exception e) {
      logger.error(e.getMessage());
      return new ResponseEntity<>("Something went wrong", HttpStatus.INTERNAL_SERVER_ERROR);
    }
    final BookCatalogue addedBook = bookCatalogueService.addBookToCatalogue(book);
    return new ResponseEntity<>(addedBook.getTitle() + " added successfully", HttpStatus.CREATED);

  }


  @GetMapping()
  public ResponseEntity<List<BookCatalogue>> getAllBookCatalogues(HttpServletRequest request) {

    //verifies token
    try {
      ResponseEntity<Boolean> tokenValidation = isTokenValid(request, null, logger, discoveryClient,
          restTemplate);
      List<BookCatalogue> books = bookCatalogueService.getAllCatalogue();
      return ResponseEntity.ok(books);
    } catch (Exception e) {
      logger.error("Error validating user token");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
  }

  @GetMapping("/isbn/{isbn}")
  public ResponseEntity<BookCatalogue> getBookByIsbn(@PathVariable Long isbn,
      HttpServletRequest request) {

    logger.info("request {}", request.toString());

    try {
      final ResponseEntity<Boolean> verifyTokenResponse = isTokenValid(request, null, logger, discoveryClient, restTemplate);

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
      logger.error("Book already exists");
    }
    if (bookByIsbn == null) {
      return new ResponseEntity<>(HttpStatus.NOT_FOUND);
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




  private ResponseEntity<String> getUserRole(String username, HttpServletRequest request) {

    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<String> authResponse;

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        logger.error("Authentication service not found");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }

      ServiceInstance instance = instances.get(0);
      String authUrl = instance.getUri() + "/auth/" + username + "/role";
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<String> requestEntity = new HttpEntity<>(token, headers);
      authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, String.class);
      if (authResponse.getStatusCode() != HttpStatus.OK) {
        return new ResponseEntity<>("User not admin", HttpStatus.UNAUTHORIZED);

      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>("Error getting user role", HttpStatus.UNAUTHORIZED);

    }

  }

}
