package com.eazybooks.bookcatalogue;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;

import com.eazybooks.bookcatalogue.controller.BookCatalogueController;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.repository.BookCatalogueRepository;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import com.eazybooks.bookcatalogue.service.TokenValidationService;
import com.eazybooks.bookcatalogue.utils.RestUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockserver.client.MockServerClient;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.json.AutoConfigureJsonTesters;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.web.client.RestTemplate;

@SpringBootTest
@ExtendWith(MockitoExtension.class)
class BookcatalogueApplicationTests {

    @InjectMocks
    private BookCatalogueController bookCatalogueController;

    @Mock
    private BookCatalogueService bookCatalogueService;

    @Mock
    private DiscoveryClient discoveryClient;

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private Logger logger;

    private MockHttpServletRequest request;

    @Mock
    private TokenValidationService tokenValidationService;


  @BeforeEach
    void setUp() {
      request = new MockHttpServletRequest();
      request.addHeader("Authorization", "Bearer eyJhbGciOiJIUzM4NCJ9.eyJzdWIiOiJ0ZXN0IiwiaWF0IjoxNzM2OTYwODY0LCJleHAiOjE3MzcwNDcyNjR9.G-1BoMvpzbCkSyOiHVxou_vxmLyBtK-X-k4_kugO6NiF1I8CHeu_UrGQL3mEdT2i");
    }

    @Test
    void testAddBookToCatalogues_Success() {
      // Arrange
      String username = "adminUser";
      BookCatalogue newBook = new BookCatalogue();
      newBook.setAuthor("username");
      newBook.setTitle("title");
      newBook.setDescription("description");
      newBook.setIsbn(Long.valueOf("1234567890"));
      newBook.setTitle("Test Book");

      // Mocking dependencies
      ResponseEntity<Boolean> tokenValidation = new ResponseEntity<>(true, HttpStatus.OK);
       ResponseEntity<String> userRole = new ResponseEntity<>("ADMIN", HttpStatus.OK);

      System.out.println("tokenValidation.getBody()"+ tokenValidation.getBody());

      Mockito.when(tokenValidationService.isTokenValid( username, request))
          .thenReturn(tokenValidation);

      Mockito.when(tokenValidationService.getUserRole(username, request))
          .thenReturn(userRole);

      Mockito.when(bookCatalogueService.getBookByIsbn(newBook.getIsbn())).thenReturn(null);
      Mockito.when(bookCatalogueService.addBookToCatalogue(newBook)).thenReturn(newBook);
//
//      Mockito.when(restTemplate.exchange(Mockito.anyString(), Mockito.eq(HttpMethod.POST), Mockito.any(), Mockito.eq(Boolean.class)))
//          .thenReturn(tokenValidation);
//      Mockito.when(bookCatalogueService.getBookByIsbn(Mockito.anyLong())).thenReturn(null);

      // Act
      ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(username, newBook, request);

      // Assert
      Assertions.assertEquals(HttpStatus.CREATED, response.getStatusCode());
      Assertions.assertEquals("Test Book added successfully", response.getBody());
    }
//
//    @Test
//    void testAddBookToCatalogues_InvalidToken() {
//      // Arrange
//      String username = "adminUser";
//      BookCatalogue newBook = new BookCatalogue();
//      newBook.setIsbn("1234567890");
//      newBook.setTitle("Test Book");
//
//      ResponseEntity<Boolean> tokenValidation = new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
//
//      Mockito.when(RestUtils.isTokenValid(request, username, logger, discoveryClient, restTemplate))
//          .thenReturn(tokenValidation);
//
//      // Act
//      ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(username, newBook, request);
//
//      // Assert
//      Assertions.assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
//      Assertions.assertEquals("Error validating token", response.getBody());
//    }
//
//    @Test
//    void testAddBookToCatalogues_NonAdminUser() {
//      // Arrange
//      String username = "regularUser";
//      BookCatalogue newBook = new BookCatalogue();
//      newBook.setIsbn("1234567890");
//      newBook.setTitle("Test Book");
//
//      ResponseEntity<Boolean> tokenValidation = new ResponseEntity<>(true, HttpStatus.OK);
//      ResponseEntity<String> userRole = new ResponseEntity<>("USER", HttpStatus.OK);
//
//      Mockito.when(RestUtils.isTokenValid(request, username, logger, discoveryClient, restTemplate))
//          .thenReturn(tokenValidation);
//      Mockito.when(restTemplate.exchange(Mockito.anyString(), Mockito.eq(HttpMethod.POST), Mockito.any(), Mockito.eq(Boolean.class)))
//          .thenReturn(tokenValidation);
//      Mockito.when(restTemplate.exchange(Mockito.anyString(), Mockito.eq(HttpMethod.GET), Mockito.any(), Mockito.eq(String.class)))
//          .thenReturn(userRole);
//
//      // Act
//      ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(username, newBook, request);
//
//      // Assert
//      Assertions.assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
//      Assertions.assertEquals("Only admin can add new book", response.getBody());
//    }
//
//    @Test
//    void testAddBookToCatalogues_BookAlreadyExists() {
//      // Arrange
//      String username = "adminUser";
//      BookCatalogue existingBook = new BookCatalogue();
//      existingBook.setIsbn("1234567890");
//      existingBook.setTitle("Existing Book");
//
//      ResponseEntity<Boolean> tokenValidation = new ResponseEntity<>(true, HttpStatus.OK);
//      ResponseEntity<String> userRole = new ResponseEntity<>("ADMIN", HttpStatus.OK);
//
//      Mockito.when(RestUtils.isTokenValid(request, username, logger, discoveryClient, restTemplate))
//          .thenReturn(tokenValidation);
//      Mockito.when(bookCatalogueService.getBookByIsbn(existingBook.getIsbn())).thenReturn(existingBook);
//
//      // Act
//      ResponseEntity<String> response = bookCatalogueController.addBookToCatalogues(username, existingBook, request);
//
//      // Assert
//      Assertions.assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
//      Assertions.assertEquals("Book already exist", response.getBody());
//    }
  }
