package com.eazybooks.authentication.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.authentication.DTO.VerifyToken;
import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.service.AuthenticatorService;
import com.eazybooks.authentication.service.JwtService;
import jakarta.servlet.http.HttpServletRequest;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
public class AuthenticationControllerTest {

  @InjectMocks
  private AuthenticationController authenticationController;

  @Mock
  private AuthenticatorService authenticatorService;

  @Mock
  private DiscoveryClient discoveryClient;

  @Mock
  private JwtService jwtService;

  @Mock
  private RestTemplate restTemplate;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
  }



  @Test
  void signUp_UserAlreadyExists() {
    CreateAccountRequest request = createAccountRequest();

    when(authenticatorService.findByUsername(request.getUsername())).thenReturn(true);

    ResponseEntity<String> response = authenticationController.signUp(request);

    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
    assertEquals("Username already exists", response.getBody());
    verify(authenticatorService, times(0)).createUserAccount(request);
  }

  @Test
  void signUp_EmailAlreadyExists() {
    CreateAccountRequest request = createAccountRequest();

    when(authenticatorService.findByUsername(request.getUsername())).thenReturn(false);
    when(authenticatorService.findByEmail(request.getEmail())).thenReturn(true);

    ResponseEntity<String> response = authenticationController.signUp(request);

    assertEquals(HttpStatus.CONFLICT, response.getStatusCode());
    assertEquals("Email already exists", response.getBody());
    verify(authenticatorService, times(0)).createUserAccount(request);
  }

  @Test
  void signUp_InvalidRequest() {
    ResponseEntity<String> response = authenticationController.signUp(null);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals("Request is empty", response.getBody());
    verify(authenticatorService, times(0)).createUserAccount(any());
  }

  @Test
  void signUp_UserServiceNotFound() {
    CreateAccountRequest request = createAccountRequest();
    AuthenticationResponse authenticationResponse = new AuthenticationResponse("test_token", "1");

    when(authenticatorService.findByUsername(request.getUsername())).thenReturn(false);
    when(authenticatorService.findByEmail(request.getEmail())).thenReturn(false);
    when(authenticatorService.createUserAccount(request)).thenReturn(authenticationResponse);

    ResponseEntity<String> response = authenticationController.signUp(request);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    verify(authenticatorService, times(1)).createUserAccount(request);
    verify(restTemplate, times(0)).exchange(anyString(), any(), any(), any(Class.class));
  }



  @Test
  void signUp_Exception() {
    CreateAccountRequest request = createAccountRequest();

    when(authenticatorService.findByUsername(request.getUsername())).thenReturn(false);
    when(authenticatorService.findByEmail(request.getEmail())).thenReturn(false);
    when(authenticatorService.createUserAccount(request)).thenThrow(new RuntimeException("Error creating user"));

    ResponseEntity<String> response = authenticationController.signUp(request);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("Failed to create user", response.getBody());
    verify(authenticatorService, times(1)).createUserAccount(request);
  }

  @Test
  void findUserRole_Success() {
    String username = "testuser";
    String token = "test_token";
    MockHttpServletRequest request = new MockHttpServletRequest();
    request.addHeader("Authorization", "Bearer " + token);

    when(authenticatorService.isTokenValid(token)).thenReturn(true);
    when(authenticatorService.findUserByRole(username)).thenReturn("ADMIN");

    ResponseEntity<String> response = authenticationController.findUserRole(username, request);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals("ADMIN", response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(token);
    verify(authenticatorService, times(1)).findUserByRole(username);
  }

  @Test
  void findUserRole_InvalidUsername() {
    HttpServletRequest request = new MockHttpServletRequest();
    request.setAttribute("Authorization", "Bearer test_token");

    ResponseEntity<String> response = authenticationController.findUserRole(null, request);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    verify(authenticatorService, times(0)).isTokenValid(anyString());
    verify(authenticatorService, times(0)).findUserByRole(anyString());
  }

  @Test
  void findUserRole_MissingToken() {
    HttpServletRequest request = new MockHttpServletRequest();

    ResponseEntity<String> response = authenticationController.findUserRole("testuser", request);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    verify(authenticatorService, times(0)).isTokenValid(anyString());
    verify(authenticatorService, times(0)).findUserByRole(anyString());
  }

  @Test
  void findUserRole_InvalidToken() {
    String username = "testuser";
    String token = "invalid_token";

    MockHttpServletRequest request = new MockHttpServletRequest();
    request.addHeader("Authorization", "Bearer " + token);

    when(authenticatorService.isTokenValid(token)).thenReturn(false);
    ResponseEntity<String> response = authenticationController.findUserRole(username, request);


    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals("User token not valid", response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(token);
    verify(authenticatorService, times(0)).findUserByRole(anyString());
  }

  @Test
  void findUserRole_Exception() {
    String username = "testuser";
    String token = "test_token";

    MockHttpServletRequest request = new MockHttpServletRequest();
    request.addHeader("Authorization", "Bearer " + token);

    when(authenticatorService.isTokenValid(token)).thenThrow(new RuntimeException("Error validating token"));

    ResponseEntity<String> response = authenticationController.findUserRole(username, request);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("Error getting role for user ", response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(token);
    verify(authenticatorService, times(0)).findUserByRole(anyString());
  }

  @Test
  void logIn_Success() {
    LoginRequest loginRequest = new LoginRequest("testuser", "password");
    String token = "test_token";

    when(authenticatorService.authenticate(loginRequest)).thenReturn(token);

    ResponseEntity<String> response = authenticationController.logIn(loginRequest);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(token, response.getBody());
    assertNotNull(response.getHeaders().get("Authorization"));
    assertEquals("Bearer " + token, response.getHeaders().get("Authorization").get(0));
    verify(authenticatorService, times(1)).authenticate(loginRequest);
  }


  @Test
  void logIn_InvalidCredentials() {
    LoginRequest loginRequest = new LoginRequest("testuser", "wrongpassword");

    when(authenticatorService.authenticate(loginRequest)).thenReturn(null);

    ResponseEntity<String> response = authenticationController.logIn(loginRequest);

    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals("Invalid login request", response.getBody());
    verify(authenticatorService, times(1)).authenticate(loginRequest);
  }

  @Test
  void logIn_Exception() {
    LoginRequest loginRequest = new LoginRequest("testuser", "password");

    when(authenticatorService.authenticate(loginRequest)).thenThrow(new RuntimeException("Error during login"));

    ResponseEntity<String> response = authenticationController.logIn(loginRequest);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals("User log in Failed", response.getBody());
    verify(authenticatorService, times(1)).authenticate(loginRequest);
  }

  @Test
  void validateToken_Success() {
    VerifyToken verifyToken = new VerifyToken("test_token", "testuser");

    when(authenticatorService.isTokenValid(verifyToken.getToken())).thenReturn(true);

    ResponseEntity<Boolean> response = authenticationController.validateToken(verifyToken);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(true, response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(verifyToken.getToken());
   }

  @Test
  void validateToken_MissingToken() {
    VerifyToken verifyToken = new VerifyToken(null, "testuser");

    ResponseEntity<Boolean> response = authenticationController.validateToken(verifyToken);

    assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
    assertEquals(false, response.getBody());
    verify(authenticatorService, times(0)).isTokenValid(anyString());
    verify(jwtService, times(0)).extractUsername(anyString());
  }

  @Test
  void validateToken_InvalidToken() {
    VerifyToken verifyToken = new VerifyToken("invalid_token", "testuser");

    when(authenticatorService.isTokenValid(verifyToken.getToken())).thenReturn(false);

    ResponseEntity<Boolean> response = authenticationController.validateToken(verifyToken);

    assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
    assertEquals(false, response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(verifyToken.getToken());
    verify(jwtService, times(0)).extractUsername(anyString());
  }


  @Test
  void validateToken_Exception() {
    VerifyToken verifyToken = new VerifyToken("test_token", "testuser");

    when(authenticatorService.isTokenValid(verifyToken.getToken())).thenThrow(new RuntimeException("Error validating token"));

    ResponseEntity<Boolean> response = authenticationController.validateToken(verifyToken);

    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
    assertEquals(false, response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(verifyToken.getToken());
    verify(jwtService, times(0)).extractUsername(anyString());
  }

  @Test
  void validateToken_Success_NoUsername() {
    VerifyToken verifyToken = new VerifyToken("test_token", null);

    when(authenticatorService.isTokenValid(verifyToken.getToken())).thenReturn(true);

    ResponseEntity<Boolean> response = authenticationController.validateToken(verifyToken);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertEquals(true, response.getBody());
    verify(authenticatorService, times(1)).isTokenValid(verifyToken.getToken());
    verify(jwtService, times(0)).extractUsername(anyString());
  }

  private CreateAccountRequest createAccountRequest() {
    return new CreateAccountRequest("1", "testuser", "password", "test", "user", "test@example.com");
  }

  static class TestServiceInstance implements ServiceInstance {

    @Override
    public String getServiceId() {
      return "user";
    }

    @Override
    public String getInstanceId() {
      return "user";
    }

    @Override
    public String getHost() {
      return "localhost";
    }

    @Override
    public int getPort() {
      return 8080;
    }

    @Override
    public boolean isSecure() {
      return false;
    }

    @Override
    public URI getUri() {
      return URI.create("http://localhost:8080");
    }

    @Override
    public java.util.Map<String, String> getMetadata() {
      return null;
    }


  }


}
