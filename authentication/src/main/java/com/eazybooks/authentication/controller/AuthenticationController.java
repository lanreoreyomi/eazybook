package com.eazybooks.authentication.controller;

import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.model.VerifyToken;
import com.eazybooks.authentication.service.AuthenticatorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/auth")
public class AuthenticationController {

  private static final Logger logger = LoggerFactory.getLogger(AuthenticationController.class);
  AuthenticatorService authenticatorService;

  public AuthenticationController(AuthenticatorService authenticatorService) {
    this.authenticatorService = authenticatorService;

  }

  @PostMapping("/create-account")
  public ResponseEntity<String> signUp(
      @RequestBody CreateAccountRequest createAccountRequest) {  // Use @RequestBody

    if (createAccountRequest == null) {
      logger.warn("Invalid signup request: User request is null");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    if (authenticatorService.findByUsername(createAccountRequest.getUsername())) {
      logger.warn("Username '{}' is already taken", createAccountRequest.getUsername());
      return new ResponseEntity<>("Username already exists", HttpStatus.CONFLICT);
    }

    if (authenticatorService.findByEmail(createAccountRequest.getEmail())) {
      logger.warn("Email '{}' is already taken", createAccountRequest.getEmail());
      return new ResponseEntity<>("Email already exists", HttpStatus.CONFLICT);
    }

    final AuthenticationResponse authenticationResponse = authenticatorService.createUserAccount(
        createAccountRequest);

    return new ResponseEntity<>("User successfully created", HttpStatus.CREATED);
  }

  @PostMapping("/login")
  public ResponseEntity<String> logIn(@RequestBody LoginRequest loginRequest) {
    logger.info("Log in Request {}", loginRequest.toString());

    if (loginRequest == null) {
      logger.warn("Invalid login request: Log in details recieved ");
      return new ResponseEntity<String>("Log In Request is empty", HttpStatus.BAD_REQUEST);
    }

    try {

      final String token = authenticatorService.authenticate(loginRequest);

      if (token != null && !token.isEmpty()) {
        logger.info("User {} successfully logged in", loginRequest.getUsername());

        return ResponseEntity.ok()
            .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
            .body("Login successful");
      }

    } catch (Exception e) {
      logger.warn("Log in failed", e);
      return new ResponseEntity<String>("User log in Failed", HttpStatus.UNAUTHORIZED);
    }
    return null;

  }

  @PostMapping("/validate-token")
  public ResponseEntity<Boolean> validateToken(@RequestBody VerifyToken verifyToken) {

    if (verifyToken.getToken() == null || verifyToken.getToken().isBlank()) {
      logger.warn("Token is missing or blank");
      return ResponseEntity.badRequest().build();
    }

    try {
      boolean isValid = authenticatorService.isTokenValid(verifyToken.getToken());
      if (isValid) {
        logger.info("Token validated successfully");
        return ResponseEntity.ok(true);
      } else {
        logger.warn("Token validation failed");
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(false);
      }
    } catch (Exception e) {
      logger.error("Error during token validation: {}", e.getMessage(), e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(false);
    }
  }
}