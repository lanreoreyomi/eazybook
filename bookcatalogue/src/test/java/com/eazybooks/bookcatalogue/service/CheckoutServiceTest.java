package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.exceptions.*;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutItems;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutStats;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.repository.CheckoutRepository;
 import org.junit.jupiter.api.BeforeEach;
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

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import org.junit.jupiter.api.DisplayName;

@ExtendWith(MockitoExtension.class)
class CheckoutServiceTest {

 @Mock
 private CheckoutRepository checkoutRepository;

 @Mock
 private IBookCatalogue bookCatalogueService;

 @Mock
 private ICheckoutStats checkoutStatsService;

 @Mock
 private ICheckoutItems checkoutItemsService;

 @Mock
 private VerificationService verificationService;

 @InjectMocks
 private CheckoutService checkoutService;

 private VerifyToken sampleVerifyToken;
 private VerifyUser sampleVerifyUser;
 private BookCatalogue sampleBookAvailable;
 private BookCatalogue sampleBookUnavailable;
 private Checkout sampleCheckoutActive;
 private Checkout sampleCheckoutReturned;
 private CheckoutStats sampleCheckoutStatsExisting;
 private final String validToken = "Bearer valid-jwt-token";
 private final String username = "testuser";
 private final Long bookIsbnAvailable = 1234567890L;
 private final Long bookIsbnUnavailable = 1111111111L;
 private final Long otherBookIsbn = 9876543210L;

 @BeforeEach
 void setUp() {
  sampleVerifyToken = new VerifyToken(validToken, username);
  sampleVerifyUser = new VerifyUser(validToken, username);

  sampleBookAvailable = new BookCatalogue();
  sampleBookAvailable.setIsbn(bookIsbnAvailable);
  sampleBookAvailable.setTitle("Available Book");
  sampleBookAvailable.setAuthor("Test Author");
  sampleBookAvailable.setQuantityForRent(5);
  sampleBookAvailable.setAvailable(true);

  sampleBookUnavailable = new BookCatalogue();
  sampleBookUnavailable.setIsbn(bookIsbnUnavailable);
  sampleBookUnavailable.setTitle("Unavailable Book");
  sampleBookUnavailable.setAuthor("Test Author");
  sampleBookUnavailable.setQuantityForRent(0);
  sampleBookUnavailable.setAvailable(false);

  sampleCheckoutActive = new Checkout();
   sampleCheckoutActive.setIsbn(bookIsbnAvailable);
  sampleCheckoutActive.setCheckedOutBy(username);
  sampleCheckoutActive.setDateOfCheckout(LocalDate.now().minusDays(5));
  sampleCheckoutActive.setExpectedReturnDate(LocalDate.now().plusDays(9));
  sampleCheckoutActive.setReturned(false);

  sampleCheckoutReturned = new Checkout();
  sampleCheckoutReturned.setIsbn(otherBookIsbn);
  sampleCheckoutReturned.setCheckedOutBy(username);
  sampleCheckoutReturned.setDateOfCheckout(LocalDate.now().minusDays(10));
  sampleCheckoutReturned.setExpectedReturnDate(LocalDate.now().minusDays(2));
  sampleCheckoutReturned.setReturned(true);

  sampleCheckoutStatsExisting = new CheckoutStats();
  sampleCheckoutStatsExisting.setBookIsbn(bookIsbnAvailable);
  sampleCheckoutStatsExisting.setTitle("Available Book");
  sampleCheckoutStatsExisting.setTotalCheckout(3);
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
 @DisplayName("processCheckout should succeed for new stats")
 void processCheckout_Success_NewStats() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(null);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());
  when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0));
  when(checkoutStatsService.save(any(CheckoutStats.class))).thenAnswer(inv -> inv.getArgument(0));
  when(bookCatalogueService.updateBook(any(BookCatalogue.class))).thenAnswer(inv -> inv.getArgument(0));
  doNothing().when(checkoutItemsService).deleteCheckoutItemsByBookIsbn(anyLong());

  String result = checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);

  assertEquals("Book successfully checked out", result);

  verify(checkoutStatsService, times(2)).findByIsbn(bookIsbnAvailable); // Called twice
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);

  ArgumentCaptor<Checkout> checkoutCaptor = ArgumentCaptor.forClass(Checkout.class);
  verify(checkoutRepository, times(1)).save(checkoutCaptor.capture());
  assertEquals(username, checkoutCaptor.getValue().getCheckedOutBy());
  assertEquals(bookIsbnAvailable, checkoutCaptor.getValue().getIsbn());
  assertFalse(checkoutCaptor.getValue().getReturned());

  ArgumentCaptor<CheckoutStats> statsCaptor = ArgumentCaptor.forClass(CheckoutStats.class);
  verify(checkoutStatsService, times(1)).save(statsCaptor.capture());
  assertEquals(1, statsCaptor.getValue().getTotalCheckout());
  assertEquals(bookIsbnAvailable, statsCaptor.getValue().getBookIsbn());
  assertEquals(sampleBookAvailable.getTitle(), statsCaptor.getValue().getTitle());

  ArgumentCaptor<BookCatalogue> bookCaptor = ArgumentCaptor.forClass(BookCatalogue.class);
  verify(bookCatalogueService, times(1)).updateBook(bookCaptor.capture());
  assertEquals(4, bookCaptor.getValue().getQuantityForRent());

  verify(checkoutItemsService, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbnAvailable);
 }

 @Test
 @DisplayName("processCheckout should succeed for existing stats")
 void processCheckout_Success_ExistingStats() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(sampleCheckoutStatsExisting);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());
  when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0));
  when(checkoutStatsService.save(any(CheckoutStats.class))).thenAnswer(inv -> inv.getArgument(0));
  when(bookCatalogueService.updateBook(any(BookCatalogue.class))).thenAnswer(inv -> inv.getArgument(0));
  doNothing().when(checkoutItemsService).deleteCheckoutItemsByBookIsbn(anyLong());

  String result = checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);

  assertEquals("Book successfully checked out", result);

  verify(checkoutStatsService, times(2)).findByIsbn(bookIsbnAvailable);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);

  ArgumentCaptor<Checkout> checkoutCaptor = ArgumentCaptor.forClass(Checkout.class);
  verify(checkoutRepository, times(1)).save(checkoutCaptor.capture());

  ArgumentCaptor<CheckoutStats> statsCaptor = ArgumentCaptor.forClass(CheckoutStats.class);
  verify(checkoutStatsService, times(1)).save(statsCaptor.capture());
  assertEquals(4, statsCaptor.getValue().getTotalCheckout()); // Incremented from 3

  ArgumentCaptor<BookCatalogue> bookCaptor = ArgumentCaptor.forClass(BookCatalogue.class);
  verify(bookCatalogueService, times(1)).updateBook(bookCaptor.capture());
  assertEquals(4, bookCaptor.getValue().getQuantityForRent());

  verify(checkoutItemsService, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbnAvailable);
 }

 @Test
 @DisplayName("processCheckout should throw InvalidUserRequestException for null input")
 void processCheckout_NullInput_ThrowsInvalidUserRequestException() {
  Runnable test1 = () -> {
    try {
      checkoutService.processCheckout(null, bookIsbnAvailable);
    } catch (BookNotFoundException e) {
      throw new RuntimeException(e);
    } catch (AuthorizationHeaderNotFound e) {
      throw new RuntimeException(e);
    }
  };
  Runnable test2 = () -> {
    try {
      checkoutService.processCheckout(sampleVerifyToken, null);
    } catch (BookNotFoundException e) {
      throw new RuntimeException(e);
    } catch (AuthorizationHeaderNotFound e) {
      throw new RuntimeException(e);
    }
  };

  InvalidUserRequestException ex1 = assertThrows(InvalidUserRequestException.class, test1::run);
  InvalidUserRequestException ex2 = assertThrows(InvalidUserRequestException.class, test2::run);
  assertEquals("Request is null", ex1.getMessage());
  assertEquals("Request is null", ex2.getMessage());
  verifyNoInteractions(checkoutStatsService, bookCatalogueService, checkoutRepository, checkoutItemsService);
 }



 @Test
 @DisplayName("processCheckout should throw BookNotEligibleForCheckoutException if already checked out")
 void processCheckout_AlreadyCheckedOut_ThrowsBookNotEligibleForCheckoutException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(sampleCheckoutStatsExisting);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive)); // User already has this book

  BookNotEligibleForCheckoutException exception = assertThrows(BookNotEligibleForCheckoutException.class, () -> {
   checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);
  });
  assertEquals("Book already checked out", exception.getMessage());

  verify(checkoutStatsService, times(1)).findByIsbn(bookIsbnAvailable);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verifyNoMoreInteractions(checkoutRepository, checkoutStatsService, bookCatalogueService, checkoutItemsService);
 }

 @Test
 @DisplayName("processCheckout should throw InternalServerException if stats service findByIsbn fails")
 void processCheckout_StatsFindFails_ThrowsRuntimeException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenThrow(new RuntimeException("Stats DB error"));

  InternalServerException exception = assertThrows(InternalServerException.class, () -> {
   checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);
  });
  assertEquals("Error processing stats", exception.getMessage());

  verify(checkoutStatsService, times(1)).findByIsbn(bookIsbnAvailable);
  verifyNoMoreInteractions(bookCatalogueService, checkoutRepository, checkoutStatsService, checkoutItemsService);
 }

 @Test
 @DisplayName("processCheckout should throw InternalServerException if checkout repo save fails")
 void processCheckout_CheckoutSaveFails_ThrowsInternalServerException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(null);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());
  when(checkoutRepository.save(any(Checkout.class))).thenThrow(new RuntimeException("Checkout DB error"));

  InternalServerException exception = assertThrows(InternalServerException.class, () -> {
   checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);
  });
  assertEquals("Error Updating  book quantity for rent", exception.getMessage());

  verify(checkoutStatsService, times(2)).findByIsbn(bookIsbnAvailable);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(checkoutRepository, times(1)).save(any(Checkout.class)); // Save was attempted
  verifyNoMoreInteractions(checkoutStatsService, bookCatalogueService, checkoutItemsService);
 }

 @Test
 @DisplayName("processCheckout should throw InternalServerException if stats service save fails")
 void processCheckout_StatsSaveFails_ThrowsInternalServerException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(null);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());
  when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0));
  when(checkoutStatsService.save(any(CheckoutStats.class))).thenThrow(new RuntimeException("Stats Save DB error"));

  InternalServerException exception = assertThrows(InternalServerException.class, () -> {
   checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);
  });
  assertEquals("Error Updating  book quantity for rent", exception.getMessage());

  verify(checkoutStatsService, times(2)).findByIsbn(bookIsbnAvailable);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(checkoutRepository, times(1)).save(any(Checkout.class));
  verify(checkoutStatsService, times(1)).save(any(CheckoutStats.class)); // Save was attempted
  verifyNoMoreInteractions(bookCatalogueService, checkoutItemsService);
 }

 @Test
 @DisplayName("processCheckout should throw InternalServerException if book update fails")
 void processCheckout_BookUpdateFails_ThrowsInternalServerException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(null);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());
  when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0));
  when(checkoutStatsService.save(any(CheckoutStats.class))).thenAnswer(inv -> inv.getArgument(0));
  when(bookCatalogueService.updateBook(any(BookCatalogue.class))).thenThrow(new RuntimeException("Book Update DB error"));

  InternalServerException exception = assertThrows(InternalServerException.class, () -> {
   checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);
  });
  assertEquals("Error Updating  book quantity for rent", exception.getMessage());

  verify(checkoutStatsService, times(2)).findByIsbn(bookIsbnAvailable);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(checkoutRepository, times(1)).save(any(Checkout.class));
  verify(checkoutStatsService, times(1)).save(any(CheckoutStats.class));
  verify(bookCatalogueService, times(1)).updateBook(any(BookCatalogue.class)); // Update was attempted
  verifyNoMoreInteractions(checkoutItemsService);
 }

 @Test
 @DisplayName("processCheckout should throw InternalServerException if checkout items delete fails")
 void processCheckout_CheckoutItemsDeleteFails_ThrowsInternalServerException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(checkoutStatsService.findByIsbn(bookIsbnAvailable)).thenReturn(null);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(Collections.emptyList());
  when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0));
  when(checkoutStatsService.save(any(CheckoutStats.class))).thenAnswer(inv -> inv.getArgument(0));
  when(bookCatalogueService.updateBook(any(BookCatalogue.class))).thenAnswer(inv -> inv.getArgument(0));
  doThrow(new RuntimeException("Items Delete DB error")).when(checkoutItemsService).deleteCheckoutItemsByBookIsbn(anyLong());

  InternalServerException exception = assertThrows(InternalServerException.class, () -> {
   checkoutService.processCheckout(sampleVerifyToken, bookIsbnAvailable);
  });
  // This exception happens after the main try block, so it's caught by the outer layer if not handled
  // However, the current code catches Exception and throws InternalServerException("Error checking book out")
  assertEquals("Error Updating  book quantity for rent", exception.getMessage());


  verify(checkoutStatsService, times(2)).findByIsbn(bookIsbnAvailable);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(checkoutRepository, times(1)).save(any(Checkout.class));
  verify(checkoutStatsService, times(1)).save(any(CheckoutStats.class));
  verify(bookCatalogueService, times(1)).updateBook(any(BookCatalogue.class));
  verify(checkoutItemsService, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbnAvailable); // Delete was attempted
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
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive));
  when(checkoutRepository.save(any(Checkout.class))).thenAnswer(inv -> inv.getArgument(0));
  when(bookCatalogueService.updateBook(any(BookCatalogue.class))).thenAnswer(inv -> inv.getArgument(0));

  String result = checkoutService.handleBookReturns(username, bookIsbnAvailable, sampleVerifyToken);

  assertEquals("Book successfully returned", result);

  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
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
      checkoutService.handleBookReturns(null, bookIsbnAvailable, sampleVerifyToken);
    } catch (BookNotFoundException e) {
      throw new RuntimeException(e);
    } catch (AuthorizationHeaderNotFound e) {
      throw new RuntimeException(e);
    }
  };
  Runnable test2 = () -> {
    try {
      checkoutService.handleBookReturns(username, null, sampleVerifyToken);
    } catch (BookNotFoundException e) {
      throw new RuntimeException(e);
    } catch (AuthorizationHeaderNotFound e) {
      throw new RuntimeException(e);
    }
  };
  Runnable test3 = () -> {
    try {
      checkoutService.handleBookReturns(username, bookIsbnAvailable, null);
    } catch (BookNotFoundException e) {
      throw new RuntimeException(e);
    } catch (AuthorizationHeaderNotFound e) {
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
  when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.FALSE);

  UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
   checkoutService.handleBookReturns(username, bookIsbnAvailable, sampleVerifyToken);
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
  when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
  when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.FALSE);

  InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
   checkoutService.handleBookReturns(username, bookIsbnAvailable, sampleVerifyToken);
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
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(null);

  BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
   checkoutService.handleBookReturns(username, bookIsbnAvailable, sampleVerifyToken);
  });
  assertEquals("Book not found", exception.getMessage());
  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, never()).findCheckoutsByCheckedOutBy(anyString());
 }

 @Test
 @DisplayName("handleBookReturns should throw BookNotEligibleForReturnException if no matching checkout found")
 void handleBookReturns_NoMatchingCheckout_ThrowsBookNotEligibleForReturnException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
  when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutReturned));

  BookNotEligibleForReturnException exception = assertThrows(BookNotEligibleForReturnException.class, () -> {
   checkoutService.handleBookReturns(username, bookIsbnAvailable, sampleVerifyToken);
  });
  assertEquals("Book mut be checked out to be returned", exception.getMessage());
  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(checkoutRepository, never()).save(any());
  verify(bookCatalogueService, never()).updateBook(any());
 }

 @Test
 @DisplayName("handleBookReturns should throw BookNotEligibleForReturnException if book already returned")
 void handleBookReturns_AlreadyReturned_ThrowsBookNotEligibleForReturnException() throws AuthorizationHeaderNotFound, BookNotFoundException {
  sampleCheckoutActive.setReturned(true);
  sampleCheckoutActive.setExpectedReturnDate(LocalDate.now().minusDays(1));

  when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
  when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, bookIsbnAvailable)).thenReturn(sampleBookAvailable);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive));

  BookNotEligibleForReturnException exception = assertThrows(BookNotEligibleForReturnException.class, () -> {
   checkoutService.handleBookReturns(username, bookIsbnAvailable, sampleVerifyToken);
  });
  assertTrue(exception.getMessage().startsWith("Book has already been  returned on "));
  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, bookIsbnAvailable);
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(checkoutRepository, never()).save(any());
  verify(bookCatalogueService, never()).updateBook(any());
 }

 @Test
 @DisplayName("getCheckoutInfo should return list of CheckoutInfo successfully")
 void getCheckoutInfo_Success() throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
  when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(List.of(sampleCheckoutActive, sampleCheckoutReturned));
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleCheckoutActive.getIsbn())).thenReturn(sampleBookAvailable);
  BookCatalogue otherBook = new BookCatalogue();
  otherBook.setIsbn(otherBookIsbn);
  otherBook.setTitle("Other Book");
  when(bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleCheckoutReturned.getIsbn())).thenReturn(otherBook);

  List<CheckoutInfo> result = checkoutService.getCheckoutInfo(sampleVerifyToken, username);

  assertNotNull(result);
  assertEquals(2, result.size());

  CheckoutInfo info1 = result.stream().filter(i -> i.getBookIsbn().equals(sampleCheckoutActive.getIsbn())).findFirst().orElse(null);
  assertNotNull(info1);
  assertEquals(sampleBookAvailable.getTitle(), info1.getNameOfBook());
  assertEquals(sampleCheckoutActive.getIsbn(), info1.getBookIsbn());
  assertEquals(sampleCheckoutActive.getDateOfCheckout(), LocalDate.parse(info1.getCheckoutDate()));
  assertEquals(sampleCheckoutActive.getReturned(), info1.getReturnStatus());
  assertEquals(sampleCheckoutActive.getExpectedReturnDate(), LocalDate.parse(info1.getExpectedReturnDate()));

  CheckoutInfo info2 = result.stream().filter(i -> i.getBookIsbn().equals(sampleCheckoutReturned.getIsbn())).findFirst().orElse(null);
  assertNotNull(info2);
  assertEquals(otherBook.getTitle(), info2.getNameOfBook());
  assertEquals(sampleCheckoutReturned.getIsbn(), info2.getBookIsbn());
  assertEquals(sampleCheckoutReturned.getDateOfCheckout(), LocalDate.parse(info2.getCheckoutDate()));
  assertEquals(sampleCheckoutReturned.getReturned(),  info2.getReturnStatus());
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
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenReturn(null);

  List<CheckoutInfo> result = checkoutService.getCheckoutInfo(sampleVerifyToken, username);

  assertNull(result);
  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
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
  assertEquals("Book not found", exception.getMessage());

  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(bookCatalogueService, times(1)).getBookByIsbn(sampleVerifyToken, sampleCheckoutActive.getIsbn());
 }

 @Test
 @DisplayName("getCheckoutInfo should throw InternalServerException if repository call fails")
 void getCheckoutInfo_RepositoryThrowsException_ThrowsInternalServerException()
     throws AuthorizationHeaderNotFound, BookNotFoundException {
  when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
  when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
  when(checkoutRepository.findCheckoutsByCheckedOutBy(username)).thenThrow(new RuntimeException("Database error"));

  InternalServerException exception = assertThrows(InternalServerException.class, () -> {
   checkoutService.getCheckoutInfo(sampleVerifyToken, username);
  });
  assertEquals("Database error", exception.getMessage());

  verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
  verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
  verify(checkoutRepository, times(1)).findCheckoutsByCheckedOutBy(username);
  verify(bookCatalogueService, never()).getBookByIsbn(any(), anyLong());
 }
}