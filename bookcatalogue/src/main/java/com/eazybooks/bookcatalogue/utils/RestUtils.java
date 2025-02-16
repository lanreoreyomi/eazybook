package com.eazybooks.bookcatalogue.utils;

import com.eazybooks.bookcatalogue.model.SERVICES;
import com.eazybooks.bookcatalogue.model.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;
import org.slf4j.Logger;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

public class RestUtils {

  private static ResponseEntity<Boolean> verfiyToken(HttpServletRequest request, String username, Logger logger,
      DiscoveryClient discoveryClient, RestTemplate restTemplate) {
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
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      logger.info("Service url: "+ authUrl);
      final String s = username != null ? username : null;
      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token,s ), headers);
      authResponse = restTemplate.exchange(
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


  public static ResponseEntity<Boolean> isTokenValid(HttpServletRequest request, String username,
      Logger logger, DiscoveryClient discoveryClient, RestTemplate restTemplate) {
    return verfiyToken(request, username, logger, discoveryClient, restTemplate);

  }
}
