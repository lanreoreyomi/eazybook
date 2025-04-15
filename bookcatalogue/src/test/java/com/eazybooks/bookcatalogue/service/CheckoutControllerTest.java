package com.eazybooks.bookcatalogue.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.ICheckoutItems;
import com.eazybooks.bookcatalogue.service.CheckoutService;
import com.eazybooks.bookcatalogue.service.IcheckoutStats;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
class CheckoutControllerTest {
//
//  @InjectMocks
//  private CheckoutController checkoutController;
//
//  @Mock
//  private CheckoutService checkoutService;
//
//  @Mock
//  private BookCatalogueService bookCatalogueService;
//
//  @Mock
//  private IcheckoutStats checkoutStatsService;
//  @Mock
//  private ICheckoutItems checkoutItemsService;
//
//  @Mock
//  private RestTemplate restTemplate;
//
//  @Mock
//  private VerificationService verificationService;
//
//  @Test
//  void checkout_Success() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//    book.setQuantityForRent(2);
//
//    final String validToken = getValidToken();
//
//    Checkout checkout = new Checkout();
//    checkout.setIsbn(book.getIsbn());
//    checkout.setDateOfCheckout(LocalDate.now());
//    checkout.setExpectedReturnDate(LocalDate.now().plusWeeks(2));
//    checkout.setReturned(false);
//    checkout.setCheckedOutBy(username);
//
//    CheckoutStats checkoutStats = new CheckoutStats();
//
//    checkoutStats.setBookIsbn(bookIsbn);
//    checkoutStats.setTotalCheckout(0);
//    checkoutStats.setTitle(book.getTitle());
//
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//    when(checkoutStatsService.findByIsbn(bookIsbn)).thenReturn(checkoutStats);
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(new ArrayList<>());
//
//    when(checkoutService.save(any(Checkout.class))).thenAnswer(invocation -> {
//      Checkout savedCheckout = invocation.getArgument(0);
//      savedCheckout.setId(String.valueOf(1L)); // Simulate setting an ID during save
//      return savedCheckout;
//    });
//
//    ArgumentCaptor<Checkout> checkoutCaptor = ArgumentCaptor.forClass(Checkout.class);
//    ArgumentCaptor<CheckoutStats> checkoutStatsCaptor = ArgumentCaptor.forClass(CheckoutStats.class);
//    ArgumentCaptor<BookCatalogue> bookCaptor = ArgumentCaptor.forClass(BookCatalogue.class);
//
//
//
//    ResponseEntity<String> response = checkoutController.checkout(username, bookIsbn, request);
//
//    System.out.println("Response: "+ response.getBody());
//    assertEquals(HttpStatus.CREATED, response.getStatusCode());
//    assertEquals("Book successfully checked out", response.getBody());
//    verify(checkoutService, times(1)).save(checkoutCaptor.capture());
//
//    Checkout capturedCheckout = checkoutCaptor.getValue();
//    assertEquals(bookIsbn, capturedCheckout.getIsbn());
//    assertEquals(username, capturedCheckout.getCheckedOutBy());
//    assertNotNull(capturedCheckout.getDateOfCheckout());
//    assertNotNull(capturedCheckout.getExpectedReturnDate());
//    assertEquals(false, capturedCheckout.getReturned());
//
//    verify(checkoutStatsService, times(1)).save(checkoutStatsCaptor.capture());
//    CheckoutStats capturedCheckoutStats = checkoutStatsCaptor.getValue();
//    assertEquals(bookIsbn, capturedCheckoutStats.getBookIsbn());
//    assertEquals(1, capturedCheckoutStats.getTotalCheckout());
//
//    verify(bookCatalogueService, times(1)).updateBook(bookCaptor.capture());
//    BookCatalogue capturedBook = bookCaptor.getValue();
//    assertEquals(1, capturedBook.getQuantityForRent());
//
//    verify(checkoutItemsService, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbn);
//  }
//
//  @Test
//  void checkout_AlreadyCheckedOut() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//    book.setQuantityForRent(2);
//
//    Checkout checkout = new Checkout();
//    checkout.setIsbn(bookIsbn);
//    checkout.setCheckedOutBy(username);
//    List<Checkout> checkouts = new ArrayList<>();
//    checkouts.add(checkout);
//
//    final String validToken = getValidToken();
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(checkouts);
//
//    ResponseEntity<String> response = checkoutController.checkout(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
//    assertEquals("Book already checked out", response.getBody());
//    verify(checkoutService, times(0)).save(any(Checkout.class));
//    verify(checkoutStatsService, times(0)).save(any(CheckoutStats.class));
//    verify(bookCatalogueService, times(0)).updateBook(any(BookCatalogue.class));
//  }
//
//  @Test
//  void checkout_BookNotFound() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//
//    final String validToken = getValidToken();
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(null);
//
//    ResponseEntity<String> response = checkoutController.checkout(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
//    assertEquals("Book not found", response.getBody());
//  }
//
//  @Test
//  void checkout_QuantityZero() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//    book.setQuantityForRent(0);
//    final String validToken = getValidToken();
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//
//    ResponseEntity<String> response = checkoutController.checkout(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
//    assertEquals("Book not available for checkout. Checkout is max out", response.getBody());
//  }
//
//
//  @Test
//  void checkout_TokenInvalid() {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer invalid_token");
//
//    ResponseEntity<Boolean> tokenResponse = ResponseEntity.ok(false);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(tokenResponse);
//
//    ResponseEntity<String> response = checkoutController.checkout(username, bookIsbn, request);
//    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
//  }
//
//  @Test
//  void returnBook_Success() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//    book.setQuantityForRent(0);
//
//    Checkout checkout = new Checkout();
//    checkout.setIsbn(bookIsbn);
//    checkout.setCheckedOutBy(username);
//    checkout.setReturned(false);
//
//    List<Checkout> checkouts = new ArrayList<>();
//    checkouts.add(checkout);
//    final String validToken = getValidToken();
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(checkouts);
//
//    ResponseEntity<String> response = checkoutController.returnBook(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.OK, response.getStatusCode());
//    assertEquals("Book successfully returned", response.getBody());
//    verify(bookCatalogueService, times(1)).updateBook(any(BookCatalogue.class));
//    verify(checkoutService, times(1)).updateCheckout(any(Checkout.class));
//  }
//
//  @Test
//  void returnBook_BookNotFound() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer valid_token");
//    final String validToken = getValidToken();
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(null);
//
//    ResponseEntity<String> response = checkoutController.returnBook(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
//    assertEquals("Book not found", response.getBody());
//  }
//
//  @Test
//  void returnBook_BookAlreadyReturned() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//    book.setQuantityForRent(0);
//
//    Checkout checkout = new Checkout();
//    checkout.setIsbn(bookIsbn);
//    checkout.setCheckedOutBy(username);
//    checkout.setReturned(true);
//    checkout.setExpectedReturnDate(LocalDate.now());
//
//    List<Checkout> checkouts = new ArrayList<>();
//    checkouts.add(checkout);
//    final String validToken = getValidToken();
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(checkouts);
//
//    ResponseEntity<String> response = checkoutController.returnBook(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
//    assertEquals("Book has already been  returned on " + checkout.getExpectedReturnDate(),
//        response.getBody());
//  }
//
//  @Test
//  void returnBook_NotCheckedOutByUser() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//
//    List<Checkout> checkouts = new ArrayList<>();
//    final String validToken = getValidToken();
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(checkouts);
//
//    ResponseEntity<String> response = checkoutController.returnBook(username, bookIsbn, request);
//
//    assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
//    assertEquals("Book mut be checked out to be returned", response.getBody());
//  }
//
//  @Test
//  void returnBook_TokenInvalid() {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer invalid_token");
//
//    ResponseEntity<Boolean> tokenResponse = ResponseEntity.ok(false);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(tokenResponse);
//
//    ResponseEntity<String> response = checkoutController.returnBook(username, bookIsbn, request);
//    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
//  }
//
//  @Test
//  void getCheckoutHistory_Success() throws BookNotFoundException {
//    String username = "testuser";
//    Long bookIsbn = 123L;
//    BookCatalogue book = createBook(bookIsbn);
//    Checkout checkout = new Checkout();
//    checkout.setIsbn(bookIsbn);
//    checkout.setCheckedOutBy(username);
//    List<Checkout> checkouts = new ArrayList<>();
//    checkouts.add(checkout);
//    final String validToken = getValidToken();
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer " + validToken);
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
//
//    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(checkouts);
//    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
//
//    ResponseEntity<List<CheckoutInfo>> response = checkoutController.getCheckoutHistory(username,
//        request);
//
//    assertEquals(HttpStatus.OK, response.getStatusCode());
//    assertNotNull(response.getBody());
//    assertEquals(1, response.getBody().size());
//    verify(checkoutService, times(1)).findCheckoutsByCheckedOutBy(username);
//    verify(bookCatalogueService, times(1)).getBookByIsbn(anyLong());
//  }

  public BookCatalogue createBook(Long isbn) {
    BookCatalogue book = new BookCatalogue();
    book.setTitle("Test Title");
    book.setAuthor(" Test author");
    book.setAvailable(true);
    book.setIsbn(isbn);
    book.setId(String.valueOf(isbn));
    book.setPublicationYear(2020);
    return book;
  }

  public String getValidToken(){
    return "valid_token";
  }
}