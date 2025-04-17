package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.DTO.VerifyToken;
import com.eazybooks.wishlist.DTO.VerifyUser;
import com.eazybooks.wishlist.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.wishlist.exceptions.BookExistException;
import com.eazybooks.wishlist.exceptions.BookNotFoundException;
import com.eazybooks.wishlist.exceptions.InvalidUserRequestException;
import com.eazybooks.wishlist.exceptions.InvalidUserTokenException;
import com.eazybooks.wishlist.exceptions.UserNotFoundException;
import com.eazybooks.wishlist.model.BookCatalogue;
import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.Wishlist;
import com.eazybooks.wishlist.repository.WishlistRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class WishlistServiceTest {

  @Mock
  private WishlistRepository wishlistRepository;

  @Mock
  private VerificationService verificationService;

  @InjectMocks
  private WishlistService wishlistService;

  private VerifyToken sampleVerifyToken;
  private VerifyUser sampleVerifyUser;
  private CreateWishListRequest sampleWishlistRequest;
  private BookCatalogue sampleBookCatalogue;
  private Wishlist sampleWishlistItem;
  private final String validToken = "Bearer valid-jwt-token";
  private final String username = "testuser";
  private final Long bookIsbn = 1234567890L;
  private final Long wishlistId = 1L;

  @BeforeEach
  void setUp() {
    sampleVerifyToken = new VerifyToken(validToken, username);
    sampleVerifyUser = new VerifyUser(validToken, username);
    sampleWishlistRequest = new CreateWishListRequest(bookIsbn);

    sampleBookCatalogue = new BookCatalogue(bookIsbn);
    sampleBookCatalogue.setIsbn(bookIsbn);
    sampleBookCatalogue.setTitle("Test Book Title");
    sampleBookCatalogue.setAuthor("Test Author");

    sampleWishlistItem = new Wishlist();
     sampleWishlistItem.setIsbn(bookIsbn);
    sampleWishlistItem.setBookTitle("Test Book Title");
    sampleWishlistItem.setUsername(username);
    sampleWishlistItem.setLocalDate(LocalDate.now());
  }

  @Test
  @DisplayName("adBookToWishlist should add book successfully")
  void adBookToWishlist_Success() throws AuthorizationHeaderNotFound, BookNotFoundException, BookExistException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyBookIsbn(any(VerifyToken.class), anyLong())).thenReturn(sampleBookCatalogue);
    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, username)).thenReturn(Optional.empty());
    when(wishlistRepository.save(any(Wishlist.class))).thenReturn(sampleWishlistItem);

    String result = wishlistService.adBookToWishlist(sampleVerifyToken, sampleWishlistRequest);

    assertEquals("Added "+ sampleBookCatalogue.getTitle() +" to wishlist", result);

    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyBookIsbn(sampleVerifyToken, bookIsbn);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, username);

    ArgumentCaptor<Wishlist> captor = ArgumentCaptor.forClass(Wishlist.class);
    verify(wishlistRepository, times(1)).save(captor.capture());
    Wishlist savedWishlist = captor.getValue();
    assertEquals(bookIsbn, savedWishlist.getIsbn());
    assertEquals(sampleBookCatalogue.getTitle(), savedWishlist.getBookTitle());
    assertEquals(username, savedWishlist.getUsername());
    assertEquals(LocalDate.now(), LocalDate.parse(savedWishlist.getLocalDate()));
  }

  @Test
  @DisplayName("adBookToWishlist should throw InvalidUserRequestException for null token")
  void adBookToWishlist_NullToken_ThrowsInvalidUserRequestException() {
    InvalidUserRequestException exception = assertThrows(InvalidUserRequestException.class, () -> {
      wishlistService.adBookToWishlist(null, sampleWishlistRequest);
    });
    assertEquals("Request can not be empty", exception.getMessage());
    verifyNoInteractions(verificationService, wishlistRepository);
  }

  @Test
  @DisplayName("adBookToWishlist should throw exception if token verification fails")
  void adBookToWishlist_TokenVerificationFails_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      wishlistService.adBookToWishlist(sampleVerifyToken, sampleWishlistRequest);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verifyNoMoreInteractions(verificationService);
    verifyNoInteractions(wishlistRepository);
  }

  @Test
  @DisplayName("adBookToWishlist should throw BookNotFoundException if book verification fails")
  void adBookToWishlist_BookVerificationFails_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyBookIsbn(any(VerifyToken.class), anyLong())).thenThrow(new BookNotFoundException("Book not found via service"));

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      wishlistService.adBookToWishlist(sampleVerifyToken, sampleWishlistRequest);
    });
    assertEquals("Book not found via service", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyBookIsbn(sampleVerifyToken, bookIsbn);
    verifyNoInteractions(wishlistRepository);
  }

  @Test
  @DisplayName("adBookToWishlist should throw BookExistException if book already in wishlist")
  void adBookToWishlist_BookExists_ThrowsBookExistException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyBookIsbn(any(VerifyToken.class), anyLong())).thenReturn(sampleBookCatalogue);
    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, username)).thenReturn(Optional.of(sampleWishlistItem));

    BookExistException exception = assertThrows(BookExistException.class, () -> {
      wishlistService.adBookToWishlist(sampleVerifyToken, sampleWishlistRequest);
    });
    assertEquals("Book already added to wishlist", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyBookIsbn(sampleVerifyToken, bookIsbn);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, username);
    verify(wishlistRepository, never()).save(any());
  }

  @Test
  @DisplayName("adBookToWishlist should throw BookExistException if repository find throws exception")
  void adBookToWishlist_RepoFindThrowsException_ThrowsBookExistException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyBookIsbn(any(VerifyToken.class), anyLong())).thenReturn(sampleBookCatalogue);
    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, username)).thenThrow(new RuntimeException("DB error"));

    BookExistException exception = assertThrows(BookExistException.class, () -> {
      wishlistService.adBookToWishlist(sampleVerifyToken, sampleWishlistRequest);
    });
    assertEquals("Book already added to wishlist", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyBookIsbn(sampleVerifyToken, bookIsbn);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, username);
    verify(wishlistRepository, never()).save(any());
  }

  @Test
  @DisplayName("findById should return null")
  void findById_ReturnsNull() {
    assertNull(wishlistService.findById(wishlistId));
    verifyNoInteractions(wishlistRepository);
  }

  @Test
  @DisplayName("findByBookIsbn should return wishlist item when found")
  void findByBookIsbn_Found_ReturnsWishlistItem() {
    when(wishlistRepository.findByIsbn(bookIsbn)).thenReturn(Optional.of(sampleWishlistItem));
    Wishlist result = wishlistService.findByBookIsbn(bookIsbn);
    assertNotNull(result);
    assertEquals(sampleWishlistItem, result);
    verify(wishlistRepository, times(1)).findByIsbn(bookIsbn);
  }

  @Test
  @DisplayName("findByBookIsbn should return null when not found")
  void findByBookIsbn_NotFound_ReturnsNull() {
    when(wishlistRepository.findByIsbn(bookIsbn)).thenReturn(Optional.empty());
    Wishlist result = wishlistService.findByBookIsbn(bookIsbn);
    assertNull(result);
    verify(wishlistRepository, times(1)).findByIsbn(bookIsbn);
  }

  @Test
  @DisplayName("findByBookIsbnAndUsername should return wishlist item when found")
  void findByBookIsbnAndUsername_Found_ReturnsWishlistItem() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, username)).thenReturn(Optional.of(sampleWishlistItem));

    Wishlist result = wishlistService.findByBookIsbnAndUsername(sampleVerifyToken, bookIsbn);

    assertNotNull(result);
    assertEquals(sampleWishlistItem, result);
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, username);
  }

  @Test
  @DisplayName("findByBookIsbnAndUsername should throw InvalidUserRequestException for null input")
  void findByBookIsbnAndUsername_NullInput_ThrowsInvalidUserRequestException() {
    Runnable test1 = () -> {
      try {
        wishlistService.findByBookIsbnAndUsername(null, bookIsbn);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test2 = () -> {
      try {
        wishlistService.findByBookIsbnAndUsername(sampleVerifyToken, null);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };

    InvalidUserRequestException ex1 = assertThrows(InvalidUserRequestException.class, test1::run);
    InvalidUserRequestException ex2 = assertThrows(InvalidUserRequestException.class, test2::run);
    assertEquals("Request can not be empty", ex1.getMessage());
    assertEquals("Request can not be empty", ex2.getMessage());
    verifyNoInteractions(verificationService, wishlistRepository);
  }

  @Test
  @DisplayName("findByBookIsbnAndUsername should throw exception if token verification fails")
  void findByBookIsbnAndUsername_TokenVerificationFails_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      wishlistService.findByBookIsbnAndUsername(sampleVerifyToken, bookIsbn);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verifyNoInteractions(wishlistRepository);
  }

  @Test
  @DisplayName("findByBookIsbnAndUsername should throw BookNotFoundException when item not found")
  void findByBookIsbnAndUsername_NotFound_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, username)).thenReturn(Optional.empty());

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      wishlistService.findByBookIsbnAndUsername(sampleVerifyToken, bookIsbn);
    });
    assertEquals("Book not found in wishlist", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, username);
  }

  @Test
  @DisplayName("deleteById should do nothing")
  void deleteById_DoesNothing() {
    wishlistService.deleteById(wishlistId);
    verifyNoInteractions(wishlistRepository);
  }

  @Test
  @DisplayName("removeByBookIsbn should remove item successfully")
  void removeByBookIsbn_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);

    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, sampleVerifyToken.getUsername()))
        .thenReturn(Optional.of(sampleWishlistItem)); // Simulate book exists
    doNothing().when(wishlistRepository).deleteByIsbn(anyLong());

    String result = wishlistService.removeByBookIsbn(sampleVerifyToken, bookIsbn);

    assertEquals("Removed from wishlist", result);
    verify(verificationService, times(2)).verifyUserToken(sampleVerifyToken);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, sampleVerifyToken.getUsername()); // Called by internal findByBookIsbn
    verify(wishlistRepository, times(1)).deleteByIsbn(bookIsbn);
  }

  @Test
  @DisplayName("removeByBookIsbn should throw InvalidUserRequestException for null input")
  void removeByBookIsbn_NullInput_ThrowsInvalidUserRequestException() {
    Runnable test1 = () -> {
      try {
        wishlistService.removeByBookIsbn(null, bookIsbn);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test2 = () -> {
      try {
        wishlistService.removeByBookIsbn(sampleVerifyToken, null);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };

    InvalidUserRequestException ex1 = assertThrows(InvalidUserRequestException.class, test1::run);
    InvalidUserRequestException ex2 = assertThrows(InvalidUserRequestException.class, test2::run);
    assertEquals("Request can not be empty", ex1.getMessage());
    assertEquals("Request can not be empty", ex2.getMessage());
    verifyNoInteractions(verificationService, wishlistRepository);
  }

  @Test
   void removeByBookIsbn_TokenVerificationFails_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      wishlistService.removeByBookIsbn(sampleVerifyToken, bookIsbn);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verifyNoInteractions(wishlistRepository);
  }


  @Test
   void removeByBookIsbn_FindThrowsException_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(wishlistRepository.findByIsbnAndUsername(bookIsbn, sampleVerifyToken.getUsername())).thenThrow(new RuntimeException("DB error"));

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      wishlistService.removeByBookIsbn(sampleVerifyToken, bookIsbn);
    });
    assertEquals("Book not found in wishlist", exception.getMessage());
    verify(verificationService, times(2)).verifyUserToken(sampleVerifyToken);
    verify(wishlistRepository, times(1)).findByIsbnAndUsername(bookIsbn, sampleVerifyToken.getUsername());
    verify(wishlistRepository, never()).deleteByIsbn(anyLong());
  }

  @Test
   void findByUserName_Success_ReturnsList() throws AuthorizationHeaderNotFound {
    List<Wishlist> expectedList = List.of(sampleWishlistItem);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(wishlistRepository.findByUsername(username)).thenReturn(Optional.of(expectedList));

    List<Wishlist> result = wishlistService.findByUserName(sampleVerifyToken);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(expectedList, result);
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(wishlistRepository, times(1)).findByUsername(username);
  }

  @Test
   void findByUserName_NotFound_ReturnsNull() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(wishlistRepository.findByUsername(username)).thenReturn(Optional.empty());

    List<Wishlist> result = wishlistService.findByUserName(sampleVerifyToken);

    assertNull(result);
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(wishlistRepository, times(1)).findByUsername(username);
  }

  @Test
   void findByUserName_NullToken_ThrowsInvalidUserRequestException() {
    InvalidUserRequestException exception = assertThrows(InvalidUserRequestException.class, () -> {
      wishlistService.findByUserName(null);
    });
    assertEquals("Request can not be empty", exception.getMessage());
    verifyNoInteractions(verificationService, wishlistRepository);
  }

  @Test
  @DisplayName("findByUserName should throw exception if token verification fails")
  void findByUserName_TokenVerificationFails_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      wishlistService.findByUserName(sampleVerifyToken);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, never()).verifyUserExists(any());
    verifyNoInteractions(wishlistRepository);
  }

  @Test
  @DisplayName("findByUserName should throw exception if user verification fails")
  void findByUserName_UserVerificationFails_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenThrow(new UserNotFoundException("User invalid"));

    UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      wishlistService.findByUserName(sampleVerifyToken);
    });
    assertEquals("User invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verifyNoInteractions(wishlistRepository);
  }
}