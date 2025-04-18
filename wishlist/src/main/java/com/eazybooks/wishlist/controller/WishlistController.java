package com.eazybooks.wishlist.controller;
import com.eazybooks.wishlist.DTO.VerifyToken;
import com.eazybooks.wishlist.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.wishlist.exceptions.BookExistException;
import com.eazybooks.wishlist.exceptions.BookNotFoundException;
import com.eazybooks.wishlist.exceptions.InvalidUserRequestException;
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
import java.util.Objects;
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

  public WishlistController(IWishlist wishlistService) {
    this.wishlistService = wishlistService;
   }
  @PostMapping("{username}/add")
  public ResponseEntity<String> addBookToWishlist(@PathVariable String username,
      @RequestBody CreateWishListRequest wishListRequest, HttpServletRequest request)
      throws BookNotFoundException, AuthorizationHeaderNotFound, BookExistException {

    if (Objects.isNull(username) || Objects.isNull(wishListRequest) || Objects.isNull(request)) {
      logger.error("Username can not be empty");
      throw new InvalidUserRequestException("Username can not be empty");
    }

     VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"),
        username);


    final String response = wishlistService.adBookToWishlist(verifyTokenRequest, wishListRequest);
    return new ResponseEntity<>(String.format(response), HttpStatus.CREATED);
  }


  @PostMapping("/{username}/remove")
  public ResponseEntity<String> removeBookFromWishlist(@PathVariable String username,
      @RequestBody RemoveBookFromWishListRequest removeRequest, HttpServletRequest request)
      throws AuthorizationHeaderNotFound, BookNotFoundException {

    if (Objects.isNull(username) || Objects.isNull(removeRequest) || Objects.isNull(request)) {
      logger.error("User request can not be empty");
      throw new InvalidUserRequestException("User request can not be empty");
    }

   VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);
    final String response = wishlistService.removeByBookIsbn(verifyTokenRequest, removeRequest.getIsbn());
    return new ResponseEntity<>(
        String.format(response), HttpStatus.OK);
  }

  @GetMapping("/{username}/all")
  public ResponseEntity<List<Wishlist>> getAllWishList(@PathVariable String username,
      HttpServletRequest request) throws AuthorizationHeaderNotFound {


    if (Objects.isNull(username) || Objects.isNull(request)) {
      logger.error("User request can not be empty");
      throw new InvalidUserRequestException("User request can not be empty");
    }

    VerifyToken verifyTokenRequest = new VerifyToken(request.getHeader("Authorization"), username);

    final List<Wishlist> wishlists = wishlistService.findByUserName(verifyTokenRequest);

    wishlists.forEach(wishlist -> {
      logger.info("Found wishlist {}", wishlist.toString());
    });

    return new ResponseEntity<>(wishlists, HttpStatus.OK);
  }

}
