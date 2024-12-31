package com.eazybooks.bookcatalogue.utils;

import com.eazybooks.bookcatalogue.model.VerifyToken;
import java.util.List;
import java.util.logging.Logger;
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

  public static ResponseEntity<String> validateRequestToken(String authHeader, String token,
      DiscoveryClient discoveryClient, RestTemplate restTemplate) {
    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }
      ServiceInstance instance = instances.get(0);

      String authUrl = instance.getUri() + "/auth/validate-token";

      HttpHeaders headers = new HttpHeaders();
      headers.set("Authorization", authHeader);
      headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

      HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token), headers);
      ResponseEntity<Boolean> authResponse = restTemplate.exchange(
          authUrl, HttpMethod.POST, requestEntity, Boolean.class);

      if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
          authResponse.getBody())) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();

      }

    }
    catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
    }
    return null;
  }

}
