package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.net.InetAddress;
import java.net.UnknownHostException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class VerificationService {

  private final Logger logger = LoggerFactory.getLogger(VerificationService.class);

  @Autowired
  RestTemplate standardRestTemplate;

  @Value("${authentication.service.url}")
  private String authenticationServiceUrl;

  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request, String username)
  {
    String authHeader = request.getHeader("Authorization");

    logger.info("authHeader");
    logger.info(request.getHeader("Authorization"));
    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

     try {
      String authUrl = authenticationServiceUrl();
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      final String s = username != null ? username : null;
      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token,s ), headers);
      authResponse = standardRestTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);
      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("Token validation failed");
        return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);

    }
  }
  private ResponseEntity<Boolean> verifyToken(HttpServletRequest request)
      throws UnknownHostException {

    logger.info("request {}", request.toString());
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }
    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

    logger.info("Authentication Service url {}", authenticationServiceUrl());
    try {
      String auth_service_url = authenticationServiceUrl();

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, null), headers);
      authResponse = standardRestTemplate.exchange(
          auth_service_url, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        logger.warn("Token validation failed");
        return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
    }
  }

  public ResponseEntity<Boolean> verifyUserToken(HttpServletRequest request, String username) {
    if (username == null) {
      logger.debug("Verifying token without username"); // Add logging
      try {
        return verifyToken(request);
      } catch (UnknownHostException e) {
        throw new RuntimeException(e);
      }
    } else {
      logger.debug("Verifying token for username: {}", username); // Add logging
      return verifyToken(request, username);
    }
  }



  private ResponseEntity<String> getUserRole(String username, HttpServletRequest request) {
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<String> authResponse;

    try {
      String authUrl = authenticationServiceUserRoleUrl(username);
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<String> requestEntity = new HttpEntity<>(token, headers);
      authResponse = standardRestTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, String.class);
      if (authResponse.getStatusCode() != HttpStatus.OK) {
        return new ResponseEntity<>("User not admin", HttpStatus.UNAUTHORIZED);

      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>("Error getting user role", HttpStatus.UNAUTHORIZED);

    }

  }

  public ResponseEntity<String> verifyUserRole(String username, HttpServletRequest request) {
    return getUserRole(username, request);
  }

  private  String authenticationServiceUserRoleUrl(String username) throws UnknownHostException {
    return authenticationServiceUrl+"/auth/"+username + "/role";
  }
  private String authenticationServiceUrl() {
    return authenticationServiceUrl+"/auth/validate-token";
  }
}
