package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.DTO.VerifyUserRole;
import com.eazybooks.bookcatalogue.enums.ROLE;
import com.eazybooks.bookcatalogue.exceptions.*;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.repository.BookCatalogueRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;


import java.util.List;
import java.util.Optional;


import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;


@ExtendWith(MockitoExtension.class)
class BookCatalogueServiceTest {


  @Mock
  private BookCatalogueRepository bookCatalogueRepository;


  @Mock
  private VerificationService verificationService;


  @InjectMocks
  private BookCatalogueService bookCatalogueService;


  private VerifyToken sampleVerifyToken;
  private VerifyUser sampleVerifyUser;
  private VerifyUserRole sampleVerifyUserRoleAdmin;
  private VerifyUserRole sampleVerifyUserRoleUser;
  private BookCatalogue sampleBook1;
  private BookCatalogue sampleBook2;
  private final String validToken = "Bearer valid-jwt-token";
  private final String username = "testuser";
  private final String adminUsername = "adminuser";
  private final Long sampleIsbn1 = 1234567890L;
  private final Long sampleIsbn2 = 9876543210L;


  @BeforeEach
  void setUp() {
    sampleVerifyToken = new VerifyToken(validToken, username);
    sampleVerifyUser = new VerifyUser(validToken, username);
    sampleVerifyUserRoleAdmin = new VerifyUserRole(adminUsername, validToken);
    sampleVerifyUserRoleUser = new VerifyUserRole(username, validToken);


    sampleBook1 = new BookCatalogue();
    sampleBook1.setIsbn(sampleIsbn1);
    sampleBook1.setTitle("Test Book One");
    sampleBook1.setAuthor("Author One");


    sampleBook2 = new BookCatalogue();
    sampleBook2.setIsbn(sampleIsbn2);
    sampleBook2.setTitle("Test Book Two");
    sampleBook2.setAuthor("Author Two");
  }

  @Test
  @DisplayName("getAllCatalogue should return list of books when token is valid")
  void getAllCatalogue_ValidToken_ReturnsBookList() throws AuthorizationHeaderNotFound {

    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueRepository.findAll()).thenReturn(List.of(sampleBook1, sampleBook2));

    List<BookCatalogue> result = bookCatalogueService.getAllCatalogue(sampleVerifyToken);

    assertNotNull(result);
    assertEquals(2, result.size());
    assertEquals(sampleBook1, result.get(0));
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, times(1)).findAll();
  }


  @Test
  @DisplayName("getAllCatalogue should throw InvalidUserRequestException when VerifyToken is null")
  void getAllCatalogue_NullToken_ThrowsInvalidUserRequestException()
      throws AuthorizationHeaderNotFound {

    VerifyToken nullToken = null;

    InvalidUserRequestException exception = assertThrows(InvalidUserRequestException.class, () -> {
      bookCatalogueService.getAllCatalogue(nullToken);
    });
    assertEquals("Verify token is null", exception.getMessage());
    verify(verificationService, never()).verifyUserToken(any());
    verify(bookCatalogueRepository, never()).findAll();
  }


  @Test
  @DisplayName("getAllCatalogue should handle token validation returning false (logs error, proceeds)")
  void getAllCatalogue_TokenValidationFalse_LogsErrorAndReturnsList() throws AuthorizationHeaderNotFound {

    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.FALSE);
    when(bookCatalogueRepository.findAll()).thenReturn(List.of(sampleBook1));
    List<BookCatalogue> result = bookCatalogueService.getAllCatalogue(sampleVerifyToken);


     assertNotNull(result);
    assertEquals(1, result.size());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, times(1)).findAll();
   }


  @Test
  void getAllCatalogue_TokenVerificationThrowsException_ThrowsInternalServerException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class)))
        .thenThrow(new AuthorizationHeaderNotFound("Auth header missing"));
    InternalServerException exception = assertThrows(InternalServerException.class, () -> {
      bookCatalogueService.getAllCatalogue(sampleVerifyToken);
    });
    assertEquals("Error validating user token", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, never()).findAll();
  }
  @Test
  @DisplayName("addBookToCatalogue should add book successfully for admin user")
  void addBookToCatalogue_AdminUser_NewBook_Success() throws AuthorizationHeaderNotFound, BookExistException {
    VerifyToken adminToken = new VerifyToken(validToken, adminUsername);
    VerifyUser adminUser = new VerifyUser(validToken, adminUsername);
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserRole(any(VerifyUserRole.class))).thenReturn(ROLE.ADMIN.toString());
    when(bookCatalogueRepository.findByBookByIsbn(sampleBook1.getIsbn())).thenReturn(Optional.empty());
    when(bookCatalogueRepository.save(any(BookCatalogue.class))).thenReturn(sampleBook1);

     BookCatalogue result = bookCatalogueService.addBookToCatalogue(adminToken, sampleBook1);

     assertNotNull(result);
    assertEquals(sampleBook1.getIsbn(), result.getIsbn());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserRole(any(VerifyUserRole.class));
    verify(bookCatalogueRepository, times(1)).findByBookByIsbn(sampleBook1.getIsbn());
    verify(bookCatalogueRepository, times(1)).save(sampleBook1);
  }


  @Test
  @DisplayName("addBookToCatalogue should throw InvalidUserRequestException for null book")
  void addBookToCatalogue_NullBook_ThrowsInvalidUserRequestException() {
    BookCatalogue nullBook = null;

    InvalidUserRequestException exception = assertThrows(InvalidUserRequestException.class, () -> {
      bookCatalogueService.addBookToCatalogue(sampleVerifyToken, nullBook);
    });
    assertEquals("Book isbn is null or request is empty", exception.getMessage());
    verifyNoInteractions(verificationService, bookCatalogueRepository);
  }


  @Test
  @DisplayName("addBookToCatalogue should throw InvalidUserRequestException for null token request")
  void addBookToCatalogue_NullTokenRequest_ThrowsInvalidUserRequestException() {
    VerifyToken nullToken = null;

    InvalidUserRequestException exception = assertThrows(InvalidUserRequestException.class, () -> {
      bookCatalogueService.addBookToCatalogue(nullToken, sampleBook1);
    });
    assertEquals("Book isbn is null or request is empty", exception.getMessage());
    verifyNoInteractions(verificationService, bookCatalogueRepository);
  }


  @Test
  @DisplayName("addBookToCatalogue should throw UserNotFoundException if user verification returns false")
  void addBookToCatalogue_UserVerificationFalse_ThrowsUserNotFoundException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.FALSE);
    UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      bookCatalogueService.addBookToCatalogue(sampleVerifyToken, sampleBook1);
    });
    assertEquals("User not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, never()).verifyUserToken(any());
    verify(verificationService, never()).verifyUserRole(any());
    verify(bookCatalogueRepository, never()).findByBookByIsbn(anyLong());
    verify(bookCatalogueRepository, never()).save(any());
  }


  @Test
  @DisplayName("addBookToCatalogue should throw UserNotFoundException if user verification throws exception")
  void addBookToCatalogue_UserVerificationThrowsException_ThrowsUserNotFoundException() throws AuthorizationHeaderNotFound {

    when(verificationService.verifyUserExists(any(VerifyUser.class)))
        .thenThrow(new InternalServerException("Verification service down"));


    UserNotFoundException exception = assertThrows(UserNotFoundException.class, () -> {
      bookCatalogueService.addBookToCatalogue(sampleVerifyToken, sampleBook1);
    });
    assertEquals("User not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, never()).verifyUserToken(any());

  }


  @Test
  @DisplayName("addBookToCatalogue should throw InvalidUserTokenException if token verification returns false")
  void addBookToCatalogue_TokenVerificationFalse_ThrowsInvalidUserTokenException() throws AuthorizationHeaderNotFound {

    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.FALSE);


    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      bookCatalogueService.addBookToCatalogue(sampleVerifyToken, sampleBook1);
    });
    assertEquals("Error validating user token", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, never()).verifyUserRole(any());
  }


  @Test
  void addBookToCatalogue_RoleVerificationThrowsException_ThrowsInternalServerException() throws AuthorizationHeaderNotFound {

    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserRole(any(VerifyUserRole.class)))
        .thenThrow(new InternalServerException("Role service down"));

    InternalServerException exception = assertThrows(InternalServerException.class, () -> {
      bookCatalogueService.addBookToCatalogue(sampleVerifyToken, sampleBook1);
    });
    assertEquals("Role service down", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserRole(any(VerifyUserRole.class));
    verify(bookCatalogueRepository, never()).findByBookByIsbn(anyLong());
    verify(bookCatalogueRepository, never()).save(any());
  }


  @Test
  @DisplayName("addBookToCatalogue should throw BookExistException if book already exists")
  void addBookToCatalogue_BookExists_ThrowsBookExistException() throws AuthorizationHeaderNotFound {
    VerifyToken adminToken = new VerifyToken(validToken, adminUsername);


    when(verificationService.verifyUserExists(any(VerifyUser.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(verificationService.verifyUserRole(any(VerifyUserRole.class))).thenReturn(ROLE.ADMIN.toString());
    when(bookCatalogueRepository.findByBookByIsbn(sampleBook1.getIsbn())).thenReturn(Optional.of(sampleBook1));


    BookExistException exception = assertThrows(BookExistException.class, () -> {
      bookCatalogueService.addBookToCatalogue(adminToken, sampleBook1);
    });
    assertEquals("Book already exists", exception.getMessage());
    verify(verificationService, times(1)).verifyUserExists(any(VerifyUser.class));
    verify(verificationService, times(1)).verifyUserToken(any(VerifyToken.class));
    verify(verificationService, times(1)).verifyUserRole(any(VerifyUserRole.class));
    verify(bookCatalogueRepository, times(1)).findByBookByIsbn(sampleBook1.getIsbn());
    verify(bookCatalogueRepository, never()).save(any());
  }

  @Test
  @DisplayName("getBookByIsbn should return book when token is valid and book exists")
  void getBookByIsbn_ValidToken_BookExists_ReturnsBook() throws AuthorizationHeaderNotFound, BookNotFoundException, InvalidUserTokenException {

    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueRepository.findByBookByIsbn(sampleIsbn1)).thenReturn(Optional.of(sampleBook1));

    BookCatalogue result = bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleIsbn1);

    assertNotNull(result);
    assertEquals(sampleBook1, result);
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, times(1)).findByBookByIsbn(sampleIsbn1);
  }


  @Test
  @DisplayName("getBookByIsbn should throw BookNotFoundException for null ISBN")
  void getBookByIsbn_NullIsbn_ThrowsBookNotFoundException() {
    Long nullIsbn = null;

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      bookCatalogueService.getBookByIsbn(sampleVerifyToken, nullIsbn);
    });
    assertEquals("Book isbn is null", exception.getMessage());
    verifyNoInteractions(verificationService, bookCatalogueRepository);
  }


  @Test
  @DisplayName("getBookByIsbn should throw InvalidUserTokenException if token validation returns false")
  void getBookByIsbn_TokenValidationFalse_ThrowsInvalidUserTokenException() throws AuthorizationHeaderNotFound {

    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.FALSE);

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleIsbn1);
    });
    assertEquals("Error validating user token", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, never()).findByBookByIsbn(anyLong());
  }


  @Test
  @DisplayName("getBookByIsbn should propagate AuthorizationHeaderNotFound from token verification")
  void getBookByIsbn_TokenVerificationThrowsAuthException_PropagatesException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class)))
        .thenThrow(new AuthorizationHeaderNotFound("Test Auth Header Missing"));

    AuthorizationHeaderNotFound exception = assertThrows(AuthorizationHeaderNotFound.class, () -> {
      bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleIsbn1);
    });
    assertEquals("Test Auth Header Missing", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, never()).findByBookByIsbn(anyLong());
  }


  @Test
  @DisplayName("getBookByIsbn should throw BookNotFoundException if book does not exist")
  void getBookByIsbn_BookNotFound_ThrowsBookNotFoundException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(bookCatalogueRepository.findByBookByIsbn(sampleIsbn1)).thenReturn(Optional.empty());

    BookNotFoundException exception = assertThrows(BookNotFoundException.class, () -> {
      bookCatalogueService.getBookByIsbn(sampleVerifyToken, sampleIsbn1);
    });
    assertEquals("Book with isbn not found", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(bookCatalogueRepository, times(1)).findByBookByIsbn(sampleIsbn1);
  }
  @Test
  @DisplayName("updateBook should save and return updated book")
  void updateBook_ValidBook_SavesAndReturnsBook() {

    BookCatalogue bookToUpdate = sampleBook1;
    bookToUpdate.setTitle("Updated Title");
    when(bookCatalogueRepository.save(any(BookCatalogue.class))).thenReturn(bookToUpdate);

    BookCatalogue result = bookCatalogueService.updateBook(bookToUpdate);
    assertNotNull(result);
    assertEquals("Updated Title", result.getTitle());
    verify(bookCatalogueRepository, times(1)).save(bookToUpdate);
    verifyNoInteractions(verificationService);
  }


  @Test
  @DisplayName("updateBook should throw EmptyBookRequestException for null book")
  void updateBook_NullBook_ThrowsEmptyBookRequestException() {
    BookCatalogue nullBook = null;

    EmptyBookRequestException exception = assertThrows(EmptyBookRequestException.class, () -> {
      bookCatalogueService.updateBook(nullBook);
    });
    assertEquals("Book isbn is null", exception.getMessage());
    verify(bookCatalogueRepository, never()).save(any());
    verifyNoInteractions(verificationService);
  }
}