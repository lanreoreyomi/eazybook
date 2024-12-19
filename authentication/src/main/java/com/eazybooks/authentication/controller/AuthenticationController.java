package com.eazybooks.authentication.controller;

import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.User;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.service.AuthenticatorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
//import org.springframework.security.access.prepost.PreAuthorize;
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
    public ResponseEntity<String> signUp(@RequestBody CreateAccountRequest createAccountRequest) {  // Use @RequestBody

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
/**
Example from Vidoe
  @PostMapping("/create-account")
  public ResponseEntity<AuthenticationResponse> signUp(@RequestBody User createAccountRequest) {  // Use @RequestBod

    final AuthenticationResponse authenticationResponse = authenticatorService.registerUser(
        createAccountRequest);
    return ResponseEntity.ok(authenticationResponse);
  }
  **/
@PostMapping("/login")
  public ResponseEntity<AuthenticationResponse> logIn(@RequestBody LoginRequest loginRequest) {
  logger.info("Log in Request {}", loginRequest.toString());

  if (loginRequest == null) {
     logger.warn("Invalid login request: Log in details recieved ");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

  final AuthenticationResponse authenticate = authenticatorService.authenticate(loginRequest);
  logger.info("Log in successful with generated token {}", authenticate.toString());
  return ResponseEntity.ok(authenticate);
  }
  }