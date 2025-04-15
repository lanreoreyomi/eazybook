package com.eazybooks.wishlist;

import com.eazybooks.wishlist.controller.WishlistController;
import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.RemoveBookFromWishListRequest;
import com.eazybooks.wishlist.model.Wishlist;
import com.eazybooks.wishlist.service.VerificationService;
import com.eazybooks.wishlist.service.WishlistService;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;



class WishlistControllerTest {

  @Mock
  private WishlistService wishlistService;

  @Mock
  private VerificationService verificationService;

  @InjectMocks
  private WishlistController wishlistController;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  void addBookToWishlist_Success() throws Exception {
    String username = "testuser";
    CreateWishListRequest request = new CreateWishListRequest(1232L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);
    BookCatalogue bookCatalogue = new BookCatalogue(1232L);
    bookCatalogue.setTitle("Test Book");
    Wishlist wishlist = new Wishlist();
    wishlist.setIsbn(1232L);
    wishlist.setBookTitle("Test Book");
    wishlist.setUsername(username);
    wishlist.setLocalDate(LocalDate.now());

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(verificationService.verifyBookIsbn(mockRequest, request.getIsbn())).thenReturn(new ResponseEntity<>(bookCatalogue, HttpStatus.OK));
    when(wishlistService.findByBookIsbnAndUsername(request.getIsbn(), username)).thenReturn(null);
    when(wishlistService.save(any(Wishlist.class))).thenReturn(wishlist);

    ResponseEntity<String> response = wishlistController.addBookToWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.CREATED, response.getStatusCode());
    assertEquals("Added Test Book to wishlist ", response.getBody());
    verify(wishlistService, times(1)).save(any(Wishlist.class));
  }

  @Test
  void addBookToWishlist_TokenValidationError() throws Exception {
    String username = "testuser";
    CreateWishListRequest request = new CreateWishListRequest(4L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED));

    ResponseEntity<String> response = wishlistController.addBookToWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Error validating token", response.getBody());
    verify(wishlistService, never()).save(any(Wishlist.class));
  }

  @Test
  void addBookToWishlist_BookNotFound() throws Exception {
    String username = "testuser";
    CreateWishListRequest request = new CreateWishListRequest(24L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(verificationService.verifyBookIsbn(mockRequest, request.getIsbn())).thenReturn(new ResponseEntity<>(HttpStatus.NOT_FOUND));

    ResponseEntity<String> response = wishlistController.addBookToWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    assertEquals("Book not found for Isbn", response.getBody());
    verify(wishlistService, never()).save(any(Wishlist.class));
  }

  @Test
  void addBookToWishlist_BookAlreadyAdded() throws Exception {
    String username = "testuser";
    CreateWishListRequest request = new CreateWishListRequest(3L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);
    Wishlist existingWishlist = new Wishlist();
    existingWishlist.setIsbn(request.getIsbn());
    existingWishlist.setUsername(username);
    BookCatalogue bookCatalogue = new BookCatalogue(3L);
    bookCatalogue.setTitle("Test Book");

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(verificationService.verifyBookIsbn(mockRequest, request.getIsbn())).thenReturn(new ResponseEntity<>(bookCatalogue, HttpStatus.OK));
    when(wishlistService.findByBookIsbnAndUsername(request.getIsbn(), username)).thenReturn(existingWishlist);

    ResponseEntity<String> response = wishlistController.addBookToWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
    assertEquals("Book already added to wishlist", response.getBody());
    verify(wishlistService, never()).save(any(Wishlist.class));
  }

  @Test
  void addBookToWishlist_ExceptionInVerificationService() throws Exception {
    String username = "testuser";
    CreateWishListRequest request = new CreateWishListRequest(6L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenThrow(new RuntimeException("Token verification failed"));

    assertThrows(RuntimeException.class, () -> {
      wishlistController.addBookToWishlist(username, request, mockRequest);
    });

    verify(wishlistService, never()).save(any(Wishlist.class));
  }

  @Test
  void addBookToWishlist_ExceptionInBookVerification() throws Exception {
    String username = "testuser";
    CreateWishListRequest request = new CreateWishListRequest(4L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(verificationService.verifyBookIsbn(mockRequest, request.getIsbn())).thenThrow(new RuntimeException("Error validating book isbn"));

    ResponseEntity<String> response = wishlistController.addBookToWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Error validating book isbn", response.getBody());
    verify(wishlistService, never()).save(any(Wishlist.class));
  }

  @Test
  void removeBookFromWishlist_Success() throws Exception {
    String username = "testuser";
    RemoveBookFromWishListRequest request = new RemoveBookFromWishListRequest(1234567890L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);
    Wishlist wishlist = new Wishlist();
    wishlist.setIsbn(1234567890L);
    wishlist.setBookTitle("Test Book");
    wishlist.setUsername(username);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(wishlistService.findByBookIsbnAndUsername(request.getIsbn(), username)).thenReturn(wishlist);

    ResponseEntity<String> response = wishlistController.removeBookFromWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("Removed Test Book from wishlist ", response.getBody());
    verify(wishlistService, times(1)).removeByBookIsbn(request.getIsbn());
  }

  @Test
  void removeBookFromWishlist_TokenValidationError() throws Exception {
    String username = "testuser";
    RemoveBookFromWishListRequest request = new RemoveBookFromWishListRequest(1234567890L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED));

    ResponseEntity<String> response = wishlistController.removeBookFromWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Error validating token", response.getBody());
    verify(wishlistService, never()).removeByBookIsbn(any(Long.class));
  }

  @Test
  void removeBookFromWishlist_BookNotInWishlist() throws Exception {
    String username = "testuser";
    RemoveBookFromWishListRequest request = new RemoveBookFromWishListRequest(1234567890L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(wishlistService.findByBookIsbnAndUsername(request.getIsbn(), username)).thenReturn(null);

    ResponseEntity<String> response = wishlistController.removeBookFromWishlist(username, request, mockRequest);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Book not in wishlist", response.getBody());
    verify(wishlistService, never()).removeByBookIsbn(any(Long.class));
  }

  @Test
  void removeBookFromWishlist_Exception() throws Exception {
    String username = "testuser";
    RemoveBookFromWishListRequest request = new RemoveBookFromWishListRequest(1234567890L);
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(wishlistService.findByBookIsbnAndUsername(request.getIsbn(), username)).thenThrow(new RuntimeException("Database error"));

    assertThrows(RuntimeException.class, () -> {
      wishlistController.removeBookFromWishlist(username, request, mockRequest);
    });
    verify(wishlistService, never()).removeByBookIsbn(any(Long.class));
  }

  @Test
  void getAllWishList_Success() throws Exception {
    String username = "testuser";
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);
    Wishlist wishlist1 = new Wishlist();
    wishlist1.setIsbn(1234567890L);
    wishlist1.setBookTitle("Test Book 1");
    wishlist1.setUsername(username);
    Wishlist wishlist2 = new Wishlist();
    wishlist2.setIsbn(987654321L);
    wishlist2.setBookTitle("Test Book 2");
    wishlist2.setUsername(username);
    List<Wishlist> wishlists = Arrays.asList(wishlist1, wishlist2);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(true, HttpStatus.OK));
    when(wishlistService.findByUserName(username)).thenReturn(wishlists);

    ResponseEntity<List<Wishlist>> response = wishlistController.getAllWishList(username, mockRequest);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(wishlists, response.getBody());
  }

  @Test
  void getAllWishList_TokenValidationError() throws Exception {
    String username = "testuser";
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenReturn(new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED));

    ResponseEntity<List<Wishlist>> response = wishlistController.getAllWishList(username, mockRequest);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals(null, response.getBody());
    verify(wishlistService, never()).findByUserName(anyString());
  }
  @Test
  void getAllWishList_Exception() throws Exception {
    String username = "testuser";
    HttpServletRequest mockRequest = mock(HttpServletRequest.class);

    when(verificationService.verifyUserToken(mockRequest, null)).thenThrow(new RuntimeException("Token verification failed"));

    assertThrows(RuntimeException.class, () -> {
      wishlistController.getAllWishList(username, mockRequest);
    });

    verify(wishlistService, never()).findByUserName(anyString());
  }
}