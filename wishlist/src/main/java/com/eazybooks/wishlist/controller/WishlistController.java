package com.eazybooks.wishlist.controller;

import static com.eazybooks.wishlist.utils.RestUtils.get_AUTH_ValidateTokenUrl;
import static com.eazybooks.wishlist.utils.RestUtils.get_BOOK_IsbnUrl;

import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.RemoveBookFromWishListRequest;
import com.eazybooks.wishlist.model.VerifyToken;
import com.eazybooks.wishlist.model.Wishlist;
import com.eazybooks.wishlist.service.WishlistService;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/wishlist")
@CrossOrigin(origins = "http://localhost:5173") // Allow requests from this origin
public class WishlistController {

  private static final Logger logger = LoggerFactory.getLogger(WishlistController.class);
  private final WishlistService wishlistService;

  @Autowired
  RestTemplate standardRestTemplate;

  public WishlistController(WishlistService wishlistService) {
    this.wishlistService = wishlistService;

  }

  @PostMapping("/{username}/add")
  public ResponseEntity<String> addBookToWishlist(@PathVariable String username,
      @RequestBody CreateWishListRequest wishListRequest, HttpServletRequest request) {

    ResponseEntity<Boolean> tokenValidation = null;
    //verifies token
    try {
      tokenValidation = verifyToken(request);
    } catch (Exception e) {
      logger.error(e.getMessage());
    }

    if (!Boolean.TRUE.equals(tokenValidation.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
    }

    final ResponseEntity<BookCatalogue> bookValidation;

    try {
      logger.info("Validating book isbn");
      bookValidation = verifyBookIsbn(request, wishListRequest.getIsbn());
    } catch (Exception e) {
      return new ResponseEntity<>("Error validating book isbn", HttpStatus.BAD_REQUEST);
    }

    if (bookValidation.getStatusCode() == HttpStatus.NOT_FOUND) {
      logger.info("Book not found for Isbn {}", wishListRequest.getIsbn());
      return new ResponseEntity<>("Book not found for Isbn", HttpStatus.NOT_FOUND);
    }
    //Find if book already added to wishlist
    try {
      Wishlist byBookIsbn = wishlistService.findByBookIsbnAndUsername(wishListRequest.getIsbn(),
          username);
      logger.info("Book found for Isbn {}", wishListRequest.getIsbn());
      if (byBookIsbn != null) {
        return new ResponseEntity<>("Book already added to wishlist", HttpStatus.CONFLICT);
      }
    } catch (Exception e) {
      throw new RuntimeException(e);
    }

    LocalDate localDate = LocalDate.now();
    Wishlist wishlist = new Wishlist();
    wishlist.setIsbn(wishListRequest.getIsbn());
    wishlist.setBookTitle(bookValidation.getBody().getTitle());
    wishlist.setUsername(username);
    wishlist.setLocalDate(localDate);

    final Wishlist createdWishList = wishlistService.save(wishlist);

    return new ResponseEntity<>(
        String.format("Added %s to wishlist ", createdWishList.getBookTitle()), HttpStatus.CREATED);
  }


  @PostMapping("/{username}/remove")
  public ResponseEntity<String> removeBookFromWishlist(@PathVariable String username,
      @RequestBody RemoveBookFromWishListRequest removeRequest, HttpServletRequest request) {

    ResponseEntity<Boolean> tokenValidation = null;
    //verifies token
    try {
      tokenValidation = verifyToken(request);

    } catch (Exception e) {
      logger.error(e.getMessage());
    }

    if (!Boolean.TRUE.equals(tokenValidation.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
    }


    Wishlist byBookIsbn;
    try {
      byBookIsbn = wishlistService.findByBookIsbnAndUsername(removeRequest.getIsbn(), username);
      if (byBookIsbn == null) {
        return new ResponseEntity<>("Book not in wishlist", HttpStatus.BAD_REQUEST);
      }
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    wishlistService.removeByBookIsbn(removeRequest.getIsbn());
    return new ResponseEntity<>(
        String.format("Removed %s from wishlist ", byBookIsbn.getBookTitle()), HttpStatus.OK);
  }

  @GetMapping("/{username}/all")
  public ResponseEntity<List<Wishlist>> getAllWishList(@PathVariable String username,
      HttpServletRequest request) {
    ResponseEntity<Boolean> tokenValidation = null;
    //verifies token
    try {
      tokenValidation = verifyToken(request);

    } catch (Exception e) {
      logger.error(e.getMessage());
    }

    if (!Boolean.TRUE.equals(tokenValidation.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
    }

    final List<Wishlist> wishlists = wishlistService.findByUserName(username);

    wishlists.forEach(wishlist -> {
      logger.info("Found wishlist {}", wishlist.toString());
    });

    return new ResponseEntity<>(wishlists, HttpStatus.OK);
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

      String authUrl = get_AUTH_ValidateTokenUrl();
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token), headers);
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



  private ResponseEntity<BookCatalogue> verifyBookIsbn(HttpServletRequest request, Long bookIsbn) {

    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<BookCatalogue> authResponse = null;

    try {

      String userUrl = get_BOOK_IsbnUrl(bookIsbn);
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
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
}
