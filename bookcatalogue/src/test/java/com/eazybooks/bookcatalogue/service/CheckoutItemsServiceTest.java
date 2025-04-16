package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.exceptions.*;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.CheckoutItems;
import com.eazybooks.bookcatalogue.repository.CheckoutItemsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CheckoutItemsServiceTest {

  @Mock
  private CheckoutItemsRepository checkoutItemsRepository;

  @Mock
  private IBookCatalogue bookCatalogueService;

  @Mock
  private VerificationService verificationService;

  @InjectMocks
  private CheckoutItemsService checkoutItemsService;

  private VerifyToken sampleVerifyToken;
  private VerifyUser sampleVerifyUser;
  private BookCatalogue sampleBook1;
  private BookCatalogue sampleBook2;
  private CheckoutItems sampleCheckoutItem1;
  private CheckoutItems sampleCheckoutItem2;
  private final String validToken = "Bearer valid-jwt-token";
  private final String username = "testuser";
  private final Long bookIsbn1 = 1234567890L;
  private final Long bookIsbn2 = 9876543210L;

  @BeforeEach
  void setUp() {
    sampleVerifyToken = new VerifyToken(validToken, username);
    sampleVerifyUser = new VerifyUser(validToken, username);

    sampleBook1 = new BookCatalogue();
    sampleBook1.setIsbn(bookIsbn1);
    sampleBook1.setTitle("Test Book One");
    sampleBook1.setAuthor("Author One");

    sampleBook2 = new BookCatalogue();
    sampleBook2.setIsbn(bookIsbn2);
    sampleBook2.setTitle("Test Book Two");
    sampleBook2.setAuthor("Author Two");

    sampleCheckoutItem1 = new CheckoutItems(username, bookIsbn1);
    sampleCheckoutItem2 = new CheckoutItems(username, bookIsbn2);
   }



  @Test
   void findCheckoutItemsByBookIsbn_Found_ReturnsList() {
    List<CheckoutItems> expectedList = List.of(sampleCheckoutItem1);
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.of(expectedList));
    List<CheckoutItems> result = checkoutItemsService.findCheckoutItemsByBookIsbn(bookIsbn1);
    assertNotNull(result);
    assertEquals(1, result.size());
    assertEquals(expectedList, result);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);
  }

  @Test
   void findCheckoutItemsByBookIsbn_NotFound_ThrowsNoSuchElementException() {
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.empty());
    assertThrows(NoSuchElementException.class, () -> {
      checkoutItemsService.findCheckoutItemsByBookIsbn(bookIsbn1);
    });
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);
  }

  @Test
   void findCheckoutItemsByUsername_Found_ReturnsList() {
    List<CheckoutItems> expectedList = List.of(sampleCheckoutItem1, sampleCheckoutItem2);
    when(checkoutItemsRepository.findCheckoutItemsByUsername(username)).thenReturn(Optional.of(expectedList));
    List<CheckoutItems> result = checkoutItemsService.findCheckoutItemsByUsername(username);
    assertNotNull(result);
    assertEquals(2, result.size());
    assertEquals(expectedList, result);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByUsername(username);
  }

  @Test
   void findCheckoutItemsByUsername_NotFound_ThrowsNoSuchElementException() {
    when(checkoutItemsRepository.findCheckoutItemsByUsername(username)).thenReturn(Optional.empty());
    assertThrows(NoSuchElementException.class, () -> {
      checkoutItemsService.findCheckoutItemsByUsername(username);
    });
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByUsername(username);
  }

  @Test
   void deleteCheckoutItemsByBookIsbn_DelegatesToRepository() {
    checkoutItemsService.deleteCheckoutItemsByBookIsbn(bookIsbn1);
    verify(checkoutItemsRepository, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbn1);
  }

  @Test
   void addBookItemsToCheckout_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(sampleBook1);
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.of(Collections.emptyList()));
    when(checkoutItemsRepository.save(any(CheckoutItems.class))).thenAnswer(inv -> inv.getArgument(0));

    String result = checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);

    assertEquals("Added "+ sampleBook1.getTitle() +" to checkout", result);

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);

    ArgumentCaptor<CheckoutItems> captor = ArgumentCaptor.forClass(CheckoutItems.class);
    verify(checkoutItemsRepository, times(1)).save(captor.capture());
    CheckoutItems savedItem = captor.getValue();
    assertEquals(username, savedItem.getUsername());
    assertEquals(bookIsbn1, savedItem.getBookIsbn());
  }

  @Test
   void addBookItemsToCheckout_NullInput_ThrowsInvalidUserTokenException() {
    Runnable test1 = () -> {
      try {
        checkoutItemsService.addBookItemsToCheckout(null, bookIsbn1);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test2 = () -> {
      try {
        checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, null);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };

    InvalidUserTokenException ex1 = assertThrows(InvalidUserTokenException.class, test1::run);
    InvalidUserTokenException ex2 = assertThrows(InvalidUserTokenException.class, test2::run);
    assertEquals("Invalid request from user", ex1.getMessage());
    assertEquals("Invalid request from user", ex2.getMessage());
    verifyNoInteractions(verificationService, bookCatalogueService, checkoutItemsRepository);
  }

  @Test
   void addBookItemsToCheckout_InvalidToken_ThrowsException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, never()).verifyUserExists(any());
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
  }

  @Test
  @DisplayName("addBookItemsToCheckout should throw exception if user verification fails")
  void addBookItemsToCheckout_UserNotFound_ThrowsException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenThrow(new UserNotFoundException("User invalid"));

    UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("User invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
  }


  @Test
   void addBookItemsToCheckout_BookServiceReturnsNull_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(null);

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Book not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, never()).findCheckoutItemsByBookIsbn(anyLong());
  }

  @Test
   void addBookItemsToCheckout_BookServiceThrowsException_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound,
      BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1))
        .thenThrow(new BookNotFoundException("Simulated book service error"));

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Book not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, never()).findCheckoutItemsByBookIsbn(anyLong());
  }

  @Test
  @DisplayName("addBookItemsToCheckout should throw BookExistInCheckoutException if item already exists for user")
  void addBookItemsToCheckout_ItemExists_ThrowsBookExistInCheckoutException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(sampleBook1);
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.of(List.of(sampleCheckoutItem1)));

    BookExistInCheckoutException exception = assertThrows(BookExistInCheckoutException.class, () -> {
      checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);
    });
    assertEquals(sampleBook1.getTitle() + " already in checkout", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);
    verify(checkoutItemsRepository, never()).save(any());
  }

  @Test
   void addBookItemsToCheckout_SaveFails_ThrowsInternalServerException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(sampleBook1);
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.of(Collections.emptyList()));
    when(checkoutItemsRepository.save(any(CheckoutItems.class))).thenThrow(new RuntimeException("Database error"));

    InternalServerException exception = assertThrows(InternalServerException.class, () -> {
      checkoutItemsService.addBookItemsToCheckout(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Error adding book to checkout", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);
    verify(checkoutItemsRepository, times(1)).save(any(CheckoutItems.class));
  }

  @Test
   void checkoutItemsForUser_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(checkoutItemsRepository.findCheckoutItemsByUsername(username)).thenReturn(Optional.of(List.of(sampleCheckoutItem1, sampleCheckoutItem2)));
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(sampleBook1);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn2)).thenReturn(sampleBook2);

    List<BookCatalogue> result = checkoutItemsService.checkoutItemsForUser(sampleVerifyToken);

    assertNotNull(result);
    assertEquals(2, result.size());
    assertTrue(result.contains(sampleBook1));
    assertTrue(result.contains(sampleBook2));

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByUsername(username);
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn2);
  }

  @Test
  void checkoutItemsForUser_NullInput_ThrowsInvalidUserTokenException() {
    VerifyToken nullToken = null;
    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutItemsService.checkoutItemsForUser(nullToken);
    });
    assertEquals("Invalid request from user", exception.getMessage());
    verifyNoInteractions(verificationService, checkoutItemsRepository, bookCatalogueService);
  }

  @Test
  void checkoutItemsForUser_InvalidToken_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutItemsService.checkoutItemsForUser(sampleVerifyToken);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, never()).verifyUserExists(any());
    verify(checkoutItemsRepository, never()).findCheckoutItemsByUsername(anyString());
  }

  @Test
  @DisplayName("checkoutItemsForUser should throw exception if user verification fails")
  void checkoutItemsForUser_UserNotFound_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenThrow(new UserNotFoundException("User invalid"));

    UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      checkoutItemsService.checkoutItemsForUser(sampleVerifyToken);
    });
    assertEquals("User invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(checkoutItemsRepository, never()).findCheckoutItemsByUsername(anyString());
  }

  @Test
  void checkoutItemsForUser_RepoThrowsNoSuchElement_ThrowsInternalServerException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(checkoutItemsRepository.findCheckoutItemsByUsername(username)).thenReturn(Optional.empty());

    InternalServerException exception = assertThrows(InternalServerException.class, () -> {
      checkoutItemsService.checkoutItemsForUser(sampleVerifyToken);
    });
    assertTrue(exception.getMessage().contains("No value present"));

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByUsername(username);
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
  }


  @Test
  void checkoutItemsForUser_GetBookFailsInLoop_ThrowsInternalServerException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(checkoutItemsRepository.findCheckoutItemsByUsername(username)).thenReturn(Optional.of(List.of(sampleCheckoutItem1)));
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1))
        .thenThrow(new BookNotFoundException("Simulated book error in loop"));

    InternalServerException exception = assertThrows(InternalServerException.class, () -> {
      checkoutItemsService.checkoutItemsForUser(sampleVerifyToken);
    });
    assertEquals("Simulated book error in loop", exception.getMessage());

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByUsername(username);
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
  }

  @Test
  void removeCheckoutItems_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(sampleBook1);
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.of(List.of(sampleCheckoutItem1)));
    doNothing().when(checkoutItemsRepository).deleteCheckoutItemsByBookIsbn(anyLong());

    String result = checkoutItemsService.removeCheckoutItems(sampleVerifyToken, bookIsbn1);

    assertEquals("Removed "+ sampleBook1.getTitle() +" from checkout", result);

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);
    verify(checkoutItemsRepository, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbn1);
  }

  @Test
  void removeCheckoutItems_NullInput_ThrowsInvalidUserTokenException() {
    Runnable test1 = () -> {
      try {
        checkoutItemsService.removeCheckoutItems(null, bookIsbn1);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test2 = () -> {
      try {
        checkoutItemsService.removeCheckoutItems(sampleVerifyToken, null);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      } catch (BookNotFoundException e) {
        throw new RuntimeException(e);
      }
    };

    InvalidUserTokenException ex1 = assertThrows(InvalidUserTokenException.class, test1::run);
    InvalidUserTokenException ex2 = assertThrows(InvalidUserTokenException.class, test2::run);
    assertEquals("Invalid request from user", ex1.getMessage());
    assertEquals("Invalid request from user", ex2.getMessage());
    verifyNoInteractions(verificationService, bookCatalogueService, checkoutItemsRepository);
  }

  @Test
  void removeCheckoutItems_InvalidToken_ThrowsException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutItemsService.removeCheckoutItems(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, never()).verifyUserExists(any());
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
  }

  @Test
  void removeCheckoutItems_UserNotFound_ThrowsException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenThrow(new UserNotFoundException("User invalid"));

    UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      checkoutItemsService.removeCheckoutItems(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("User invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
  }

  @Test
  void removeCheckoutItems_BookServiceReturnsNull_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(null);

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      checkoutItemsService.removeCheckoutItems(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Book not found", exception.getMessage());

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, never()).findCheckoutItemsByBookIsbn(anyLong());
    verify(checkoutItemsRepository, never()).deleteCheckoutItemsByBookIsbn(anyLong());
  }

  @Test
  void removeCheckoutItems_BookServiceThrowsException_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1))
        .thenThrow(new BookNotFoundException("Simulated book service error"));

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      checkoutItemsService.removeCheckoutItems(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Book not found", exception.getMessage());

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, never()).findCheckoutItemsByBookIsbn(anyLong());
    verify(checkoutItemsRepository, never()).deleteCheckoutItemsByBookIsbn(anyLong());
  }


  @Test
  void removeCheckoutItems_ItemNotFoundForUser_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound, BookNotFoundException {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn1)).thenReturn(sampleBook1);
    when(checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookIsbn1)).thenReturn(Optional.of(Collections.emptyList()));

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      checkoutItemsService.removeCheckoutItems(sampleVerifyToken, bookIsbn1);
    });
    assertEquals("Book not in list", exception.getMessage());

    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn1);
    verify(checkoutItemsRepository, times(1)).findCheckoutItemsByBookIsbn(bookIsbn1);
    verify(checkoutItemsRepository, never()).deleteCheckoutItemsByBookIsbn(anyLong());
  }
}