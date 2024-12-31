package com.eazybooks.wishlist.controller;

import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.VerifyToken;
import com.eazybooks.wishlist.model.Wishlist;
import com.eazybooks.wishlist.service.WishlistService;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.List;
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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/wishlist")
public class WishlistController {

  private static final Logger logger = LoggerFactory.getLogger(WishlistController.class);
  private WishlistService wishlistService;

  private DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();

  public WishlistController(WishlistService wishlistService,
      DiscoveryClient discoveryClient) {
    this.wishlistService = wishlistService;
    this.discoveryClient = discoveryClient;
  }


  @PostMapping("/{username}/addwishlist")
  public ResponseEntity<String> addBookToWishlist(@PathVariable String username,
      @RequestBody CreateWishListRequest wishListRequest, HttpServletRequest request) {
     ResponseEntity<Boolean> booleanResponseEntity = null;

     try {
        booleanResponseEntity = verifyToken(request);
    } catch (Exception e) {
      logger.error(e.getMessage());

    }

     if (Boolean.TRUE.equals(booleanResponseEntity.getBody()) != true) {
       logger.error("Error validating token");
       return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
     }

     try {
       Wishlist byBookIsbn = wishlistService.findByBookIsbn(wishListRequest.getBookIsbn());

       if (byBookIsbn != null) {
         return new ResponseEntity<>("Book already added to wishlist", HttpStatus.CONFLICT);
       }
    } catch (Exception e) {
      throw new RuntimeException(e);
    }

    LocalDate localDate = LocalDate.now();
    Wishlist wishlist = new Wishlist();
    wishlist.setBookTitle(wishListRequest.getBookTitle());
    wishlist.setBookIsbn(wishListRequest.getBookIsbn());
    wishlist.setUserId(wishListRequest.getUserId());
    wishlist.setUsername(wishListRequest.getUserName());
    wishlist.setBookCatalogueId(wishListRequest.getBookCatalogueId());
    wishlist.setLocalDate(localDate);

    final Wishlist createdWishList = wishlistService.save(wishlist);

    return new ResponseEntity<>(String.format("Added %s to wishlist ", createdWishList.getBookTitle()), HttpStatus.CREATED);
  }

  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request) {

    logger.info("request {}", request.toString());
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse = null;
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
      logger.info("authResponse getBody{}", authResponse.getBody());

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
