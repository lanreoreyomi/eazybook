package com.eazybooks.wishlist.controller;
import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.RemoveBookFromWishListRequest;
import com.eazybooks.wishlist.model.Wishlist;
import com.eazybooks.wishlist.service.VerificationService;
import com.eazybooks.wishlist.service.WishlistService;
import interfaces.IWishlist;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/wishlist")
 public class WishlistController {

  private static final Logger logger = LoggerFactory.getLogger(WishlistController.class);
  private final IWishlist wishlistService;
  private final VerificationService verificationService;

  public WishlistController(IWishlist wishlistService,
      VerificationService verificationService) {
    this.wishlistService = wishlistService;
    this.verificationService = verificationService;
  }
  @PostMapping("/{username}/add")
  public ResponseEntity<String> addBookToWishlist(@PathVariable String username,
      @RequestBody CreateWishListRequest wishListRequest, HttpServletRequest request) {

    ResponseEntity<Boolean> tokenValidation = null;
    //verifies token
    try {
      tokenValidation = verificationService.verifyUserToken(request, null);
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
      bookValidation = verificationService.verifyBookIsbn(request, wishListRequest.getIsbn());
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
      tokenValidation = verificationService.verifyUserToken(request, null);

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
      tokenValidation = verificationService.verifyUserToken(request, null);
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

}
