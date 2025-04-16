package com.eazybooks.bookcatalogue.service;
import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.exceptions.*; // Import all custom exceptions
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutItems;
 import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.repository.CheckoutRepository;
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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CheckoutServiceTest {

  @Mock
  private CheckoutRepository checkoutRepository;

  @Mock
  private IBookCatalogue bookCatalogueService;

  @Mock
  private IcheckoutStats checkoutStatsService; // Mock even if not used in tested methods

  @Mock
  private ICheckoutItems checkoutItemsService; // Mock even if not used in tested methods

  @Mock
  private VerificationService verificationService;

  @InjectMocks
  private CheckoutService checkoutService;

  private VerifyToken sampleVerifyToken;
  private VerifyUser sampleVerifyUser;
  private BookCatalogue sampleBook;
  private Checkout sampleCheckoutActive; // A checkout that is not yet returned
  private Checkout sampleCheckoutReturned; // A checkout that is already returned
  private final String validToken = "Bearer valid-jwt-token";
  private final String username = "testuser";
  private final Long bookIsbn = 1234567890L;
  private final Long otherBookIsbn = 9876543210L;

  @BeforeEach
  void setUp() {
    sampleVerifyToken = new VerifyToken(validToken, username);
    sampleVerifyUser = new VerifyUser(validToken, username);

    sampleBook = new BookCatalogue();
    sampleBook.setIsbn(bookIsbn);
    sampleBook.setTitle("Test Book");
    sampleBook.setAuthor("Test Author");
    sampleBook.setQuantityForRent(5); // Initial quantity

    sampleCheckoutActive = new Checkout();
    sampleCheckoutActive.setId(String.valueOf(1L));
    sampleCheckoutActive.setIsbn(bookIsbn);
    sampleCheckoutActive.setCheckedOutBy(username);
    sampleCheckoutActive.setDateOfCheckout(LocalDate.now().minusDays(5));
    sampleCheckoutActive.setExpectedReturnDate(LocalDate.now().plusDays(9));
    sampleCheckoutActive.setReturned(false); // Not returned yet

    sampleCheckoutReturned = new Checkout();
    sampleCheckoutReturned.setId(String.valueOf(2L));
    sampleCheckoutReturned.setIsbn(otherBookIsbn); // Different book
    sampleCheckoutReturned.setCheckedOutBy(username);
    sampleCheckoutReturned.setDateOfCheckout(LocalDate.now().minusDays(10));
    sampleCheckoutReturned.setExpectedReturnDate(LocalDate.now().minusDays(2)); // Returned in the past
    sampleCheckoutReturned.setReturned(true); // Already returned
  }


  @Test
  @DisplayName("save should delegate to repository save")
  void save_DelegatesToRepository() {
     when(checkoutRepository.save(any(Checkout.class))).thenReturn(sampleCheckoutActive);

     Checkout result = checkoutService.save(sampleCheckoutActive);

     assertNotNull(result);
    assertEquals(sampleCheckoutActive, result);
    verify(checkoutRepository, times(1)).save(sampleCheckoutActive);
  }

  @Test
  @DisplayName("updateCheckout should delegate to repository save")
  void updateCheckout_DelegatesToRepository() {
     sampleCheckoutActive.setExpectedReturnDate(LocalDate.now().plusDays(20));
    when(checkoutRepository.save(any(Checkout.class))).thenReturn(sampleCheckoutActive);

     Checkout result = checkoutService.updateCheckout(sampleCheckoutActive);

     assertNotNull(result);
    assertEquals(sampleCheckoutActive, result);
    assertEquals(LocalDate.now().plusDays(20), result.getExpectedReturnDate());
    verify(checkoutRepository, times(1)).save(sampleCheckoutActive);
  }


  @Test
  @DisplayName("findCheckoutsByCheckedOutBy should delegate to repository")
  void findCheckoutsByCheckedOutBy_DelegatesToRepository() {
     List<Checkout> expectedList = List.of(sampleCheckoutActive, sampleCheckoutReturned);
    when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(expectedList);

     List<Checkout> result = checkoutService.findCheckoutsByCheckedOutBy(username);

     assertNotNull(result);
    assertEquals(2, result.size());
    assertEquals(expectedList, result);
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  }

  @Test
  @DisplayName("findCheckoutsByCheckedOutBy should return empty list when repository returns empty")
  void findCheckoutsByCheckedOutBy_ReturnsEmptyList() {
     when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());

     List<Checkout> result = checkoutService.findCheckoutsByCheckedOutBy(username);

     assertNotNull(result);
    assertTrue(result.isEmpty());
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  }


  @Test
  @DisplayName("handleBookReturns should successfully return a book")
  void handleBookReturns_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
     when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn)).thenReturn(sampleBook);
     when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive));
     when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0)); // Return the saved checkout
    when(bookCatalogueService.updateBook(any(BookCatalogue.class))).thenAnswer(inv -> inv.getArgument(0)); // Return the updated book

     String result = checkoutService.handleBookReturns(username, bookIsbn, sampleVerifyToken);

     assertEquals("Book successfully returned", result);

     verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
     verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn);
     verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);

     ArgumentCaptor<Checkout> checkoutCaptor = ArgumentCaptor.forClass(Checkout.class);
    verify(checkoutRepository, times(1)).save(checkoutCaptor.capture());
    Checkout updatedCheckout = checkoutCaptor.getValue();
    assertTrue(updatedCheckout.getReturned());
    assertEquals(LocalDate.now(), updatedCheckout.getExpectedReturnDate());

     ArgumentCaptor<BookCatalogue> bookCaptor = ArgumentCaptor.forClass(BookCatalogue.class);
    verify(bookCatalogueService, times(1)).updateBook(bookCaptor.capture());
    BookCatalogue updatedBook = bookCaptor.getValue();
    assertEquals(6, updatedBook.getQuantityForRent());
  }

  @Test
  @DisplayName("handleBookReturns should throw InvalidUserRequestException for null input")
  void handleBookReturns_NullInput_ThrowsInvalidUserRequestException() {
     Runnable test1 = () -> {
      try {
        checkoutService.handleBookReturns(null, bookIsbn, sampleVerifyToken);
      } catch (BookNotFoundException | AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test2 = () -> {
      try {
        checkoutService.handleBookReturns(username, null, sampleVerifyToken);
      } catch (BookNotFoundException | AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test3 = () -> {
      try {
        checkoutService.handleBookReturns(username, bookIsbn, null);
      } catch (BookNotFoundException | AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      }
    };

     InvalidUserRequestException ex1 = assertThrows(InvalidUserRequestException.class, test1::run);
    InvalidUserRequestException ex2 = assertThrows(InvalidUserRequestException.class, test2::run);
    InvalidUserRequestException ex3 = assertThrows(InvalidUserRequestException.class, test3::run);

    assertEquals("User request is empty", ex1.getMessage());
    assertEquals("User request is empty", ex2.getMessage());
    assertEquals("User request is empty", ex3.getMessage());

    verifyNoInteractions(verificationService, bookCatalogueService, checkoutRepository);
  }

  @Test
  @DisplayName("handleBookReturns should throw UserNotFoundException if user check fails")
  void handleBookReturns_UserNotFound_ThrowsUserNotFoundException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.FALSE); // User check fails

     UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      checkoutService.handleBookReturns(username, bookIsbn, sampleVerifyToken);
    });
    assertEquals("User not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, never()).verifyUserToken(any());
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
    verify(checkoutRepository, never()).findCheckoutsByCheckedOutBy(anyString());
    verify(checkoutRepository, never()).save(any());
    verify(bookCatalogueService, never()).updateBook(any());
  }

  @Test
  @DisplayName("handleBookReturns should throw InvalidUserTokenException if token validation fails")
  void handleBookReturns_InvalidToken_ThrowsInvalidUserTokenException()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE); // User exists
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.FALSE); // Token invalid

     InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutService.handleBookReturns(username, bookIsbn, sampleVerifyToken);
    });
    assertEquals("Error validating user token", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
   }

  @Test
  @DisplayName("handleBookReturns should throw BookNotFoundException if book service returns null")
  void handleBookReturns_BookServiceReturnsNull_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn)).thenReturn(null); // Book service returns null

     BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      checkoutService.handleBookReturns(username, bookIsbn, sampleVerifyToken);
    });
    assertEquals("Book not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn);
    verify(checkoutRepository, never()).findCheckoutsByCheckedOutBy(anyString());
   }

  @Test
  @DisplayName("handleBookReturns should throw BookNotEligibleForReturnException if no matching checkout found")
  void handleBookReturns_NoMatchingCheckout_ThrowsBookNotEligibleForReturnException() throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn)).thenReturn(sampleBook);
     when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutReturned)); // Only the already returned one

     BookNotEligibleForReturnException exception = assertThrows(BookNotEligibleForReturnException.class, () -> {
      checkoutService.handleBookReturns(username, bookIsbn, sampleVerifyToken);
    });
    assertEquals("Book mut be checked out to be returned", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn);
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
    verify(checkoutRepository, never()).save(any());
    verify(bookCatalogueService, never()).updateBook(any());
  }

  @Test
  @DisplayName("handleBookReturns should throw BookNotEligibleForReturnException if book already returned")
  void handleBookReturns_AlreadyReturned_ThrowsBookNotEligibleForReturnException() throws AuthorizationHeaderNotFound, BookNotFoundException {
     sampleCheckoutActive.setReturned(true); // Mark the checkout as already returned
    sampleCheckoutActive.setExpectedReturnDate(LocalDate.now().minusDays(1)); // Set a past return date

    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbn)).thenReturn(sampleBook);
     when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive));

     BookNotEligibleForReturnException exception = assertThrows(BookNotEligibleForReturnException.class, () -> {
      checkoutService.handleBookReturns(username, bookIsbn, sampleVerifyToken);
    });
    assertTrue(exception.getMessage().startsWith("Book has already been  returned on "));
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbn);
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
    verify(checkoutRepository, never()).save(any()); // No update should happen
    verify(bookCatalogueService, never()).updateBook(any()); // No update should happen
  }


  @Test
  @DisplayName("getCheckoutInfo should return list of CheckoutInfo successfully")
  void getCheckoutInfo_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
     when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive, sampleCheckoutReturned));
     when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleCheckoutActive.getIsbn())).thenReturn(sampleBook);
    BookCatalogue otherBook = new BookCatalogue();
    otherBook.setIsbn(otherBookIsbn);
    otherBook.setTitle("Other Book");
    when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleCheckoutReturned.getIsbn())).thenReturn(otherBook);

    List<CheckoutInfo> result = checkoutService.getCheckoutInfo(sampleVerifyToken, username);

    assertNotNull(result);
    assertEquals(2, result.size());

     CheckoutInfo info1 = result.stream().filter(i -> i.getBookIsbn().equals(sampleCheckoutActive.getIsbn())).findFirst().orElse(null);
    assertNotNull(info1);
    assertEquals(sampleBook.getTitle(), info1.getNameOfBook());
    assertEquals(sampleCheckoutActive.getIsbn(), info1.getBookIsbn());
    assertEquals(sampleCheckoutActive.getDateOfCheckout(), LocalDate.parse(info1.getCheckoutDate()));
    assertEquals(sampleCheckoutActive.getReturned(), info1.getReturnStatus());
    assertEquals(sampleCheckoutActive.getExpectedReturnDate(), LocalDate.parse(info1.getExpectedReturnDate()));

     CheckoutInfo info2 = result.stream().filter(i -> i.getBookIsbn().equals(sampleCheckoutReturned.getIsbn())).findFirst().orElse(null);
    assertNotNull(info2);
    assertEquals(otherBook.getTitle(), otherBook.getTitle());
    assertEquals(sampleCheckoutReturned.getIsbn(), info2.getBookIsbn());
    assertEquals(sampleCheckoutReturned.getDateOfCheckout(), LocalDate.parse(info2.getCheckoutDate()));
    assertEquals(sampleCheckoutReturned.getReturned(), info2.getReturnStatus());
    assertEquals(sampleCheckoutReturned.getExpectedReturnDate(), LocalDate.parse(info2.getExpectedReturnDate()));

     verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
     verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, sampleCheckoutActive.getIsbn());
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, sampleCheckoutReturned.getIsbn());
  }

  @Test
  @DisplayName("getCheckoutInfo should throw InvalidUserRequestException for null input")
  void getCheckoutInfo_NullInput_ThrowsInvalidUserRequestException() {
     Runnable test1 = () -> {
      try {
        checkoutService.getCheckoutInfo(null, username);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      }
    };
    Runnable test2 = () -> {
      try {
        checkoutService.getCheckoutInfo(sampleVerifyToken, null);
      } catch (AuthorizationHeaderNotFound e) {
        throw new RuntimeException(e);
      }
    };

     InvalidUserRequestException ex1 = assertThrows(InvalidUserRequestException.class, test1::run);
    InvalidUserRequestException ex2 = assertThrows(InvalidUserRequestException.class, test2::run);
    assertEquals("Request is null", ex1.getMessage());
    assertEquals("Request is null", ex2.getMessage());
    verifyNoInteractions(verificationService, checkoutRepository, bookCatalogueService);
  }

  @Test
  @DisplayName("getCheckoutInfo should throw UserNotFoundException if user check fails")
  void getCheckoutInfo_UserNotFound_ThrowsUserNotFoundException() throws AuthorizationHeaderNotFound {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.FALSE);

     UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      checkoutService.getCheckoutInfo(sampleVerifyToken, username);
    });
    assertEquals("User not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, never()).verifyUserToken(any());
    verify(checkoutRepository, never()).findCheckoutsByCheckedOutBy(anyString());
  }

  @Test
  @DisplayName("getCheckoutInfo should throw InvalidUserTokenException if token validation fails")
  void getCheckoutInfo_InvalidToken_ThrowsInvalidUserTokenException() throws AuthorizationHeaderNotFound {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.FALSE);

     InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutService.getCheckoutInfo(sampleVerifyToken, username);
    });
    assertEquals("Error validating user token", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(checkoutRepository, never()).findCheckoutsByCheckedOutBy(anyString());
  }

  @Test
  @DisplayName("getCheckoutInfo should return null when repository returns null")
  void getCheckoutInfo_RepositoryReturnsNull_ReturnsNull()
      throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(null); // Repository returns null

     List<CheckoutInfo> result = checkoutService.getCheckoutInfo(sampleVerifyToken, username);

     assertNull(result); // Service code explicitly returns null in this case
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
    verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong()); // Stream mapping shouldn't happen
  }



  @Test
  @DisplayName("getCheckoutInfo should throw RuntimeException if getBookByIsbn fails inside stream")
  void getCheckoutInfo_GetBookFailsInStream_ThrowsRuntimeException() throws AuthorizationHeaderNotFound, BookNotFoundException {
     when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive));
     when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleCheckoutActive.getIsbn()))
        .thenThrow(new BookNotFoundException("Simulated book not found in stream"));

     RuntimeException exception = assertThrows(RuntimeException.class, () -> {
      checkoutService.getCheckoutInfo(sampleVerifyToken, username);
    });
     assertEquals("Book not found", exception.getMessage()); // Matches the message in the service's catch block

     verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
    verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, sampleCheckoutActive.getIsbn());
  }
}