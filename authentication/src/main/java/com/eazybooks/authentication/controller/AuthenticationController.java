package com.eazybooks.authentication.controller;

import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.model.VerifyToken;
import com.eazybooks.authentication.service.AuthenticatorService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/auth")
public class AuthenticationController {

  private static final Logger logger = LoggerFactory.getLogger(AuthenticationController.class);
  AuthenticatorService authenticatorService;
  private DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();


  public AuthenticationController(AuthenticatorService authenticatorService,
      DiscoveryClient discoveryClient) {
    this.authenticatorService = authenticatorService;
    this.discoveryClient = discoveryClient;
  }

  @PostMapping("/create-account")
  public ResponseEntity<String> signUp(
      @RequestBody CreateAccountRequest createAccountRequest) {  // Use @RequestBody

    logger.info("Create account request: {}", createAccountRequest);
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
    logger.info("Inside try block");
    final AuthenticationResponse authenticationResponse = authenticatorService.createUserAccount(
        createAccountRequest);
    try {

      logger.info("AuthenticationResponse: {}", authenticationResponse);

      List<ServiceInstance> instances = discoveryClient.getInstances("user");
      logger.info("instances in try block: {}", instances);

      if (instances.isEmpty()) {
        logger.info("Authentication service not found");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }
      logger.info("instances is not empty try block: {}", instances);

      ServiceInstance instance = instances.get(0);

      logger.info("instance  '{}'", instance);

      String authUrl = instance.getUri() + "/user/create-account";

      logger.info("Auth url: {}", authUrl);

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", "Bearer " + authenticationResponse.getToken());
      headers.setContentType(MediaType.APPLICATION_JSON);

      CreateAccountRequest creatUserRequest = new CreateAccountRequest(
          createAccountRequest.getUsername(), null,
          createAccountRequest.getFirstname(), createAccountRequest.getLastname(),
          createAccountRequest.getEmail());

      HttpEntity<CreateAccountRequest> requestEntity = new HttpEntity<>(creatUserRequest, headers);
      ResponseEntity<String> authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, String.class);

      logger.info(authResponse.getBody());
    } catch (Exception e) {
      logger.error("Error creating user account", e.getMessage());
    }
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