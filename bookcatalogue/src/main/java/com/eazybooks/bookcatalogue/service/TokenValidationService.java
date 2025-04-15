package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class TokenValidationService {

    @Autowired
    private DiscoveryClient discoveryClient;

     private RestTemplate restTemplate = new RestTemplate();

    private Logger logger = LoggerFactory.getLogger(TokenValidationService.class);

    public ResponseEntity<Boolean> isTokenValid( String username, HttpServletRequest request) {
      String authHeader = request.getHeader("Authorization");

      if (authHeader == null || !authHeader.startsWith("Bearer ")) {
        logger.warn("Authorization header missing or invalid");
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
      }

      String token = authHeader.substring(7);
      ResponseEntity<Boolean> authResponse;

      try {
        List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
        logger.info("Found {} instances of authentication service", instances.size());

        if (instances.isEmpty()) {
          logger.error("Authentication service not found");
          return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }

        ServiceInstance instance = instances.get(0);
        String authUrl = instance.getUri() + "/auth/validate-token";
        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", authHeader);
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, username), headers);
        authResponse = restTemplate.exchange(authUrl, HttpMethod.POST, requestEntity, Boolean.class);

        if (authResponse.getStatusCode() != HttpStatus.OK || Boolean.FALSE.equals(authResponse.getBody())) {
          logger.warn("Token validation failed");
          return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
        }

        return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
      } catch (Exception e) {
        logger.error("Error during token validation: " + e.getMessage());
        return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
      }
    }
  public ResponseEntity<String> getUserRole(String username, HttpServletRequest request) {

    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<String> authResponse;

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        logger.error("Authentication service not found");
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }

      ServiceInstance instance = instances.get(0);
      String authUrl = instance.getUri() + "/auth/" + username + "/role";
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<String> requestEntity = new HttpEntity<>(token, headers);
      authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, String.class);
      if (authResponse.getStatusCode() != HttpStatus.OK) {
        return new ResponseEntity<>("User not admin", HttpStatus.UNAUTHORIZED);

      }
      return new ResponseEntity<>(authResponse.getBody(), HttpStatus.OK);
    } catch (Exception e) {
      return new ResponseEntity<>("Error getting user role", HttpStatus.UNAUTHORIZED);

    }

  }

}
