package com.eazybooks.bookcatalogue.controller;


import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.VerifyToken;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.ecwid.consul.v1.ConsulClient;
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
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/bookcatalogue")
public class BookCatalogueController {

  Logger log = LoggerFactory.getLogger(BookCatalogueController.class);

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

    log.info("request {}", request.toString());
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      log.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String token = authHeader.substring(7);
    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        log.error("Authentication service not found");
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
        log.warn("Token validation failed");
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();

      }
      List<BookCatalogue> books = bookCatalogueService.getAllCatalogue();
      return ResponseEntity.ok(books);
    } catch (Exception e) {
      log.error("Error validating user token");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
  }
}