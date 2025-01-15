package com.eazybooks.authentication.controller;

import static com.eazybooks.authentication.config.RestUtils.RestUtils.get_USER_createUserUrl;

import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.model.VerifyToken;
import com.eazybooks.authentication.service.AuthenticatorService;
import com.eazybooks.authentication.service.JwtService;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/auth")
@CrossOrigin(origins = "http://localhost:5173") // Allow requests from this origin
public class AuthenticationController {

  private static final Logger logger = LoggerFactory.getLogger(AuthenticationController.class);
  private final JwtService jwtService;
  AuthenticatorService authenticatorService;

  @Autowired
  private RestTemplate standardRestTemplate;

  public AuthenticationController(AuthenticatorService authenticatorService, JwtService jwtService) {
    this.authenticatorService = authenticatorService;
    this.jwtService = jwtService;
   }

  @PostMapping("/create-account")
  public ResponseEntity<String> signUp(
      @RequestBody CreateAccountRequest createAccountRequest) {  // Use @RequestBody
    logger.info("Create account request: {}", createAccountRequest);

    if (createAccountRequest == null) {
      logger.warn("Invalid signup request: User request is null");
      return new ResponseEntity<>("Request is empty", HttpStatus.BAD_REQUEST);
    }

    if (authenticatorService.findByUsername(createAccountRequest.getUsername())) {
      logger.warn("Username '{}' is already taken", createAccountRequest.getUsername());
      return new ResponseEntity<>("Username already exists", HttpStatus.CONFLICT);
    }

    if (authenticatorService.findByEmail(createAccountRequest.getEmail())) {
      logger.warn("Email '{}' is already taken", createAccountRequest.getEmail());
      return new ResponseEntity<>("Email already exists", HttpStatus.CONFLICT);
    }

    try {
      final AuthenticationResponse authenticationResponse =
          authenticatorService.createUserAccount(createAccountRequest);
      logger.info("AuthenticationResponse: {}", authenticationResponse);

       String user_service_url =get_USER_createUserUrl();

       logger.info("User_service_url: {}", user_service_url);


      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", "Bearer " + authenticationResponse.getToken());
      headers.setContentType(MediaType.APPLICATION_JSON);
      CreateAccountRequest createUserRequest = new CreateAccountRequest(
          authenticationResponse.getUserId(),
          createAccountRequest.getUsername(),
          null,
          createAccountRequest.getFirstname(),
          createAccountRequest.getLastname(),
          createAccountRequest.getEmail());

      HttpEntity<CreateAccountRequest> requestEntity = new HttpEntity<>(createUserRequest, headers);
      ResponseEntity<String> userCreation = standardRestTemplate.exchange(
          user_service_url, HttpMethod.POST, requestEntity, String.class);

      if (userCreation.getStatusCode() == HttpStatus.CREATED) {
        return new ResponseEntity<>("User successfully created", HttpStatus.CREATED);
      } else {
        logger.error("Failed to create user in the user service: {}", userCreation.getBody());
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
            .body("Failed to create user");
      }
    } catch (HttpClientErrorException e) {
      logger.error("Client error creating user account: {}", e.getResponseBodyAsString(), e);
      authenticatorService.deleteByUsername(createAccountRequest.getUsername());
      return ResponseEntity.status(e.getStatusCode()).body("Failed to create user: " + e.getResponseBodyAsString());

    } catch (HttpServerErrorException e) {
      logger.error("Server error creating user account: {}", e.getResponseBodyAsString(), e);
      authenticatorService.deleteByUsername(createAccountRequest.getUsername());
      return ResponseEntity.status(e.getStatusCode()).body("Failed to create user: " + e.getResponseBodyAsString());

    } catch (Exception e) {
      logger.error("Error creating user account: {}", e.getMessage(), e);
      authenticatorService.deleteByUsername(createAccountRequest.getUsername());
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Failed to create user");
    }
  }

  @PostMapping("/{username}/role")
  public ResponseEntity<String> findUserRole(@PathVariable String username,
      HttpServletRequest request) {

    if (username == null || username.isBlank()) {
      logger.warn("Username is mssing or empty");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }
    String authHeader = request.getHeader("Authorization");
    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }
    String token = authHeader.substring(7);

    try {
      boolean isValid = authenticatorService.isTokenValid(token);
      if (!isValid) {
        return new ResponseEntity<>("User token not valid", HttpStatus.UNAUTHORIZED);
      }
      final String userByRole = authenticatorService.findUserByRole(username);
      return new ResponseEntity<>(userByRole, HttpStatus.OK);

    } catch (Exception e) {
      logger.error("Error finding role for user {}: {}", username, e.getMessage(), e);
      return new ResponseEntity<>("Error getting role for user ", HttpStatus.INTERNAL_SERVER_ERROR);
    }

  }

  @PostMapping("/login")
  public ResponseEntity<String> logIn(@RequestBody LoginRequest loginRequest) {
    logger.info("Log in Request {}", loginRequest.toString());

    if (loginRequest == null) {
      logger.warn("Invalid login request: Log in details recieved ");
      return new ResponseEntity<String>("Log In Request is empty", HttpStatus.BAD_REQUEST);
    }
    String token;
    try {
      token = authenticatorService.authenticate(loginRequest);
      if (token == null && token.isEmpty()) {
        logger.info("Invalid log in request for {}", loginRequest.getUsername());
        return new ResponseEntity<>("Invalid login request", HttpStatus.UNAUTHORIZED);
      }
    } catch (Exception e) {
      logger.warn("Log in failed", e);
      return new ResponseEntity<String>("User log in Failed", HttpStatus.INTERNAL_SERVER_ERROR);
    }
    return ResponseEntity.ok()
        .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
        .body("Login successful");
  }

  @PostMapping("/validate-token")
  public ResponseEntity<Boolean> validateToken(@RequestBody VerifyToken verifyToken) {

    if (verifyToken.getToken() == null || verifyToken.getToken().isBlank()) {
      logger.warn("Token is missing or blank");
      return new ResponseEntity<>(false, HttpStatus.BAD_REQUEST);
    }
    try {
      boolean isValid = authenticatorService.isTokenValid(verifyToken.getToken());
      if (!isValid) {
        logger.warn("Token validation failed");
        return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
      }
       if (verifyToken.getUsername() != null) {
        final String usernameFromToken = jwtService.extractUsername(verifyToken.getToken());
        if (!verifyToken.getUsername().equals(usernameFromToken)) {
          logger.warn("Token validation failed username in request doesnt match token");
          return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
        }
      }

      logger.info("Token validated successfully");
      return new ResponseEntity<>(true, HttpStatus.OK);
    } catch (Exception e) {
      logger.error("Error during token validation: {}", e.getMessage(), e);
      return new ResponseEntity<>(false, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

}