package com.eazybooks.bookcatalogue.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.CheckoutItemsService;
import com.eazybooks.bookcatalogue.service.CheckoutService;
import com.eazybooks.bookcatalogue.service.CheckoutStatsService;
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
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
class CheckoutControllerTest {

  @InjectMocks
  private CheckoutController checkoutController;

  @Mock
  private CheckoutService checkoutService;

  @Mock
  private BookCatalogueService bookCatalogueService;

  @Mock
  private CheckoutStatsService checkoutStatsService;
  @Mock
  private CheckoutItemsService checkoutItemsService;

  @Mock
  private RestTemplate restTemplate;

  @Mock
  private VerificationService verificationService;

  @Test
  void checkout_Success() {
    String username = "testuser";
    Long bookIsbn = 123L;
    BookCatalogue book = createBook(bookIsbn);
    book.setQuantityForRent(2);

    final String validToken = getValidToken();

    Checkout checkout = new Checkout();
    checkout.setIsbn(book.getIsbn());
    checkout.setDateOfCheckout(LocalDate.now());
    checkout.setExpectedReturnDate(LocalDate.now().plusWeeks(2));
    checkout.setReturned(false);
    checkout.setCheckedOutBy(username);

    CheckoutStats checkoutStats = new CheckoutStats();

    checkoutStats.setBookIsbn(bookIsbn);
    checkoutStats.setTotalCheckout(0);
    checkoutStats.setTitle(book.getTitle());

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
    when(checkoutStatsService.findByIsbn(bookIsbn)).thenReturn(checkoutStats);
    when(bookCatalogueService.getBookByIsbn(bookIsbn)).thenReturn(book);
    when(checkoutService.findCheckoutsByCheckedOutBy(username)).thenReturn(new ArrayList<>());

    when(checkoutService.save(any(Checkout.class))).thenAnswer(invocation -> {
      Checkout savedCheckout = invocation.getArgument(0);
      savedCheckout.setId(String.valueOf(1L)); // Simulate setting an ID during save
      return savedCheckout;
    });

    ArgumentCaptor<Checkout> checkoutCaptor = ArgumentCaptor.forClass(Checkout.class);
    ArgumentCaptor<CheckoutStats> checkoutStatsCaptor = ArgumentCaptor.forClass(CheckoutStats.class);
    ArgumentCaptor<BookCatalogue> bookCaptor = ArgumentCaptor.forClass(BookCatalogue.class);



    ResponseEntity<String> response = checkoutController.checkout(username, bookIsbn, request);

    System.out.println("Response: "+ response.getBody());
    assertEquals(HttpStatus.CREATED, response.getStatusCode());
    assertEquals("Book successfully checked out", response.getBody());
    verify(checkoutService, times(1)).save(checkoutCaptor.capture());

    Checkout capturedCheckout = checkoutCaptor.getValue();
    assertEquals(bookIsbn, capturedCheckout.getIsbn());
    assertEquals(username, capturedCheckout.getCheckedOutBy());
    assertNotNull(capturedCheckout.getDateOfCheckout());
    assertNotNull(capturedCheckout.getExpectedReturnDate());
    assertEquals(false, capturedCheckout.getReturned());

    verify(checkoutStatsService, times(1)).save(checkoutStatsCaptor.capture());
    CheckoutStats capturedCheckoutStats = checkoutStatsCaptor.getValue();
    assertEquals(bookIsbn, capturedCheckoutStats.getBookIsbn());
    assertEquals(1, capturedCheckoutStats.getTotalCheckout());

    verify(bookCatalogueService, times(1)).updateBook(bookCaptor.capture());
    BookCatalogue capturedBook = bookCaptor.getValue();
    assertEquals(1, capturedBook.getQuantityForRent());

    verify(checkoutItemsService, times(1)).deleteCheckoutItemsByBookIsbn(bookIsbn);
  }


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