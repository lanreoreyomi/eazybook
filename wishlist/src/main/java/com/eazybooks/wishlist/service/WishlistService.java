package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.DTO.VerifyToken;
import com.eazybooks.wishlist.DTO.VerifyUser;
import com.eazybooks.wishlist.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.wishlist.exceptions.BookExistException;
import com.eazybooks.wishlist.exceptions.BookNotFoundException;
import com.eazybooks.wishlist.exceptions.InvalidUserRequestException;
import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.Wishlist;
import interfaces.IWishlist;
import com.eazybooks.wishlist.repository.WishlistRepository;
import jakarta.transaction.Transactional;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class WishlistService implements IWishlist {

  final WishlistRepository wishlistRepository;
  final VerificationService verificationService;

 Logger logger = LoggerFactory.getLogger(WishlistService.class);

  public WishlistService(WishlistRepository wishlistRepository,
      VerificationService verificationService) {
    this.wishlistRepository = wishlistRepository;
    this.verificationService = verificationService;
  }

  @Override
  public String adBookToWishlist(VerifyToken verifyTokenRequest, CreateWishListRequest wishListRequest)
      throws AuthorizationHeaderNotFound, BookNotFoundException, BookExistException {

    if(Objects.isNull(verifyTokenRequest)) {
      logger.warn("Request can not be empty");
      throw new InvalidUserRequestException("Request can not be empty");
    }
    verificationService.verifyUserToken(verifyTokenRequest);
    verificationService.verifyUserExists(new VerifyUser(verifyTokenRequest.getToken(), verifyTokenRequest.getUsername()));

    final BookCatalogue bookCatalogue = verificationService.verifyBookIsbn(verifyTokenRequest,
        wishListRequest.getIsbn());


    //Find if book already added to wishlist
    try {
      final boolean bookIspresent = wishlistRepository.findByIsbnAndUsername(wishListRequest.getIsbn(),
          verifyTokenRequest.getUsername()).isPresent();
      if (bookIspresent) {
        throw new BookExistException("Book already added to wishlist");
      }

    } catch (Exception  e) {
      throw new BookExistException("Book already added to wishlist");
    }

    LocalDate localDate = LocalDate.now();
    Wishlist wishlist = new Wishlist();
    wishlist.setIsbn(wishListRequest.getIsbn());
    wishlist.setBookTitle(bookCatalogue.getTitle());
    wishlist.setUsername(verifyTokenRequest.getUsername());
    wishlist.setLocalDate(localDate);

    wishlistRepository.save(wishlist);
    logger.info("Saved wishlist");
    return  "Added "+ bookCatalogue.getTitle() +" to wishlist";

  }

  @Override
  public Wishlist findById(Long id) {
    return null;
  }

  @Override
  public Wishlist findByBookIsbn(Long bookIsbn) {
    return wishlistRepository.findByIsbn(bookIsbn).orElse(null);
  }

  @Override
  public Wishlist findByBookIsbnAndUsername(VerifyToken verifyTokenRequest, Long bookIsbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException {

    if(Objects.isNull(verifyTokenRequest) || Objects.isNull(bookIsbn)) {
      logger.warn("Request can not be empty");
      throw new InvalidUserRequestException("Request can not be empty");

    }
    verificationService.verifyUserToken(verifyTokenRequest);

    return wishlistRepository.findByIsbnAndUsername(bookIsbn, verifyTokenRequest.getUsername())
        .orElseThrow(()-> new BookNotFoundException("Book not found in wishlist"));
  }


  @Override
  public void deleteById(Long id) {
  }

  @Override
  public String  removeByBookIsbn(VerifyToken request, Long bookIsbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    if(Objects.isNull(request) || Objects.isNull(bookIsbn)) {
      logger.warn("Request can not be empty");
      throw new InvalidUserRequestException("Request can not be empty");
    }
    verificationService.verifyUserToken(request);
    verificationService.verifyUserExists(new VerifyUser(request.getToken(),
        request.getUsername()));

    try{
      findByBookIsbnAndUsername(request, bookIsbn);
    } catch (Exception e) {
      throw new BookNotFoundException("Book not found in wishlist");
    }
    wishlistRepository.deleteByIsbn(bookIsbn);
    return "Removed from wishlist";
  }

  @Override
  public List<Wishlist> findByUserName(VerifyToken verifyTokenRequest)
      throws AuthorizationHeaderNotFound {

    if(Objects.isNull(verifyTokenRequest)){
      logger.warn("Request can not be empty");
      throw new InvalidUserRequestException("Request can not be empty");
    }

    verificationService.verifyUserToken(verifyTokenRequest);
    verificationService.verifyUserExists(new VerifyUser(verifyTokenRequest.getToken(), verifyTokenRequest.getUsername()));

   return wishlistRepository.findByUsername(verifyTokenRequest.getUsername()).orElse(null);
  }


}
