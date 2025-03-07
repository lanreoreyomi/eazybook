package com.eazybooks.bookcatalogue.controller;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
class BookCatalogueControllerTest {

  @InjectMocks
  private BookCatalogueController bookCatalogueController;

  @Mock
  private BookCatalogueService bookCatalogueService;

  @Mock
  private VerificationService verificationService;

  @Test
  void addBookToCatalogues() {

    final BookCatalogue book = createBook(1);
    final String validToken = getValidToken();
    final String username = getUsername();

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
    ResponseEntity<String> verifyUserRole = ResponseEntity.status(HttpStatus.OK).body("ADMIN");

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
    when(verificationService.verifyUserRole(username, request)).thenReturn(verifyUserRole);
    when(bookCatalogueService.getBookByIsbn(book.getIsbn())).thenReturn(null);
    when(bookCatalogueService.addBookToCatalogue(book)).thenReturn(book);

    final ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(
        username, book, request);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.CREATED, response.getStatusCode());
    assertEquals(book.getTitle() + " added successfully", response.getBody());
    verify(bookCatalogueService).addBookToCatalogue(book);
    verify(bookCatalogueService).getBookByIsbn(book.getIsbn());
    verify(verificationService).verifyUserToken(request, username);
    verify(verificationService).verifyUserRole(username, request);
  }

  @Test
  void reAddingExistingToCatalogues() {

    final BookCatalogue book = createBook(1);
    final String validToken = getValidToken();
    final String username = getUsername();

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
    ResponseEntity<String> verifyUserRole = ResponseEntity.status(HttpStatus.OK).body("ADMIN");

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken); // Mock the URL method
    when(verificationService.verifyUserRole(username, request)).thenReturn(verifyUserRole);
    when(bookCatalogueService.getBookByIsbn(book.getIsbn())).thenReturn(book);


    final ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(
        username, book, request);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
    assertEquals("Book already exist", response.getBody());
    verify(bookCatalogueService).getBookByIsbn(book.getIsbn());
    verify(verificationService).verifyUserToken(request, username);
    verify(verificationService).verifyUserRole(username, request);
  }

  @Test
 void addBookToCatalogueWithInvalidToken(){

    final BookCatalogue book = createBook(1);
    final String invalidToken = getInvalidValidToken();
    final String username = getUsername();

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(false);

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + invalidToken);

    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken); // Mock the URL method

    final ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(
        username, book, request);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals("Error validating token", response.getBody());
    verify(verificationService).verifyUserToken(request, username);

  }

  @Test
 void addBookToCatalogueWithoutAdminRole(){

    final BookCatalogue book = createBook(1);
    final String validToken = getInvalidValidToken();
    final String username = getUsername();


    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
    ResponseEntity<String> verifyUserRole = ResponseEntity.status(HttpStatus.FORBIDDEN).body("USER");

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, username)).thenReturn(verifyToken);
    when(verificationService.verifyUserRole(username, request)).thenReturn(verifyUserRole);

    final ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(
        username, book, request);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
    assertEquals("Only admin can add new book", response.getBody());
    verify(verificationService).verifyUserRole(username, request);
    verify(verificationService).verifyUserToken(request, username);

  }

  @Test
  void getAllBookCatalogues() {

    final List<BookCatalogue> books = createBooks();
    final String validToken = getValidToken();

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, null)).thenReturn(verifyToken); // Mock the URL method
    when(bookCatalogueService.getAllCatalogue()).thenReturn(books);

    final ResponseEntity<List<BookCatalogue>> response = bookCatalogueController.getAllBookCatalogues(request);
    final List<BookCatalogue> body = response.getBody();

    assertEquals(body.size(), books.size());
    assertEquals(HttpStatus.OK, response.getStatusCode());
    verify(bookCatalogueService).getAllCatalogue();
    verify(verificationService).verifyUserToken(request, null);

  }

  @Test
  void getBookByIsbn() {

    final BookCatalogue book = createBook(1);
    final String validToken = getValidToken();

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, null)).thenReturn(verifyToken);
    when(bookCatalogueService.getBookByIsbn(book.getIsbn())).thenReturn(book);

    final ResponseEntity<BookCatalogue> response = bookCatalogueController.getBookByIsbn(
        book.getIsbn(), request);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(book.getId(), response.getBody().getId());

    verify(bookCatalogueService).getBookByIsbn(book.getIsbn());
    verify(verificationService).verifyUserToken(request, null);
  }

  @Test
  void getBookByInvalidIsbn() {

    final BookCatalogue book = createBook(12);
    final String validToken = getValidToken();

    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(verificationService.verifyUserToken(request, null)).thenReturn(verifyToken);
    when(bookCatalogueService.getBookByIsbn(book.getIsbn())).thenReturn(null);

    final ResponseEntity<BookCatalogue> response = bookCatalogueController.getBookByIsbn(
        book.getIsbn(), request);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    assertNull(response.getBody());

    verify(bookCatalogueService).getBookByIsbn(book.getIsbn());
    verify(verificationService).verifyUserToken(request, null);
  }

  @Test
  void getBookCatalogueById() {

    final BookCatalogue book = createBook(1);
    final String validToken = getValidToken();

    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer " + validToken);

    when(bookCatalogueService.getBookById(1L)).thenReturn(book);

    final ResponseEntity<BookCatalogue> response =
        bookCatalogueController.getBookCatalogueById( request, 1L);

    System.out.println(response.getStatusCode());
    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(response.getBody().getId(), book.getId());

    verify(bookCatalogueService).getBookById(1L);
   }

  public BookCatalogue createBook(int isbn ) {
    BookCatalogue book = new BookCatalogue();
    book.setTitle("Test Title");
    book.setAuthor(" Test author");
    book.setAvailable(true);
    book.setIsbn(Long.valueOf(isbn));
    book.setId(String.valueOf(isbn));
    book.setPublicationYear(2020);
    return book;
  }

  public List<BookCatalogue> createBooks() {

    List<BookCatalogue> books = new ArrayList<>();

    for (int i = 1; i <6;  i++){
      books.add(createBook(i));
    }
    return books;
  }

  public String getUsername() {
    return "test_user";
  }

  public String getValidToken(){
    return "valid_token";
  }

  public String getInvalidValidToken(){
    return "invalid_valid_token";
  }
}