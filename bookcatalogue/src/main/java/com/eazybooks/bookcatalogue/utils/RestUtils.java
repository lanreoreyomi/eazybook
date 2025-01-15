package com.eazybooks.bookcatalogue.utils;

import com.eazybooks.bookcatalogue.model.VerifyToken;
import jakarta.servlet.http.HttpServletRequest;
import java.net.InetAddress;
import java.net.UnknownHostException;
import org.slf4j.Logger;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

public class RestUtils {

  public static String get_AUTH_ValidateTokenUrl() throws UnknownHostException {
    return "http://"+ InetAddress.getLocalHost().getHostAddress()+":9084/auth/validate-token";
  }

  public static String get_AUTH_userRoleTokenUrl(String username) throws UnknownHostException {
     return "http://"+ InetAddress.getLocalHost().getHostAddress()+":9084/auth/"+username + "/role";
  }

  private static ResponseEntity<Boolean> verfiyToken(HttpServletRequest request, String username, Logger logger, RestTemplate restTemplate) {
    String authHeader = request.getHeader("Authorization");

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);
    ResponseEntity<Boolean> authResponse;

    try {
      String authUrl = get_AUTH_ValidateTokenUrl();
      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

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
      Logger logger, RestTemplate restTemplate) {
    return verfiyToken(request, username, logger, restTemplate);

  }

}
