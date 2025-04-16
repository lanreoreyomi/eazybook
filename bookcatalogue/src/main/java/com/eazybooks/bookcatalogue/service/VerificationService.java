package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.DTO.VerifyUser;
import com.eazybooks.bookcatalogue.DTO.VerifyUserRole;
import com.eazybooks.bookcatalogue.enums.ROLE;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.InternalServerException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.exceptions.UserNotAdminException;
import com.eazybooks.bookcatalogue.exceptions.UserNotFoundException;
import java.util.List;
import java.util.Objects;
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
public class VerificationService {

  private final Logger logger = LoggerFactory.getLogger(VerificationService.class);

  @Autowired
  private DiscoveryClient discoveryClient;

  RestTemplate restTemplate = new RestTemplate();


    private Boolean verifyToken (VerifyToken tokenRequest) throws AuthorizationHeaderNotFound {

       String authHeader = tokenRequest.getToken();

      if (authHeader == null || !authHeader.startsWith("Bearer ")) {
        logger.warn("Authorization header missing or invalid");
        throw new AuthorizationHeaderNotFound("Authorization header missing or invalid");
      }

      String token = tokenRequest.getToken().substring(7);
      ResponseEntity<Boolean> authResponse;

       try {
         List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
         logger.info("Found {} instances of authentication service", instances.size());


        ServiceInstance instance = instances.get(0);
        String authUrl = instance.getUri() + "/auth/validate-token";
        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", authHeader);
        headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

        logger.info("Service url: " + authUrl);
         HttpEntity<VerifyToken> requestEntity = new HttpEntity<>(new VerifyToken(token, tokenRequest.getUsername()), headers);
        authResponse = restTemplate.exchange(
            authUrl, HttpMethod.POST, requestEntity, Boolean.class);

        if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
            authResponse.getBody())) {
          logger.warn("Token validation failed");
          throw new InvalidUserTokenException("Token validation failed");
        }
        return true;
      } catch (Exception e) {
         throw new InvalidUserTokenException("Token validation failed");
      }
     }

     private Boolean verifyUser( VerifyUser verifyUserRequest)
         throws AuthorizationHeaderNotFound {

      String authHeader = verifyUserRequest.getToken();

       if (authHeader == null || !authHeader.startsWith("Bearer ")) {
         logger.warn("Authorization header missing or invalid");
         throw new AuthorizationHeaderNotFound("Authorization header missing or invalid");
       }

       String token = verifyUserRequest.getToken().substring(7);
       ResponseEntity<Boolean> authResponse;
       try {
         List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
         logger.info("Found {} instances of authentication service", instances.size());

         ServiceInstance instance = instances.get(0);
         String authUrl = instance.getUri() +"/auth/"+ verifyUserRequest.getUsername()+"/verify-user";

         HttpHeaders headers = new HttpHeaders();
         headers.set("Authorization", authHeader);
         headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

         logger.info("Service url: " + authUrl);
         HttpEntity<String> requestEntity = new HttpEntity<>(token, headers);

         authResponse = restTemplate.exchange(
             authUrl, HttpMethod.POST, requestEntity, Boolean.class);

         if (authResponse.getStatusCode() != HttpStatus.OK && Boolean.FALSE.equals(
             authResponse.getBody())) {
           logger.warn("User verification failed");
           throw new UserNotFoundException("User not found");
         }
         return true;
       } catch (Exception e) {
         throw new UserNotFoundException("User not found");
       }

     }
    public Boolean verifyUserToken (VerifyToken tokenRequest) throws AuthorizationHeaderNotFound {

      if (Objects.isNull(tokenRequest)) {
        logger.debug("Verifying token without username");
          throw new InternalServerException("Something went wrong verifying token");
        }

      return verifyToken(tokenRequest);
    }

    private String verifyRole (VerifyUserRole roleRequest){
      String authHeader = roleRequest.getHeader();

      if (authHeader == null || !authHeader.startsWith("Bearer ")) {
        logger.warn("Authorization header missing or invalid");
        throw new InvalidUserTokenException("Authorization header missing or invalid");
      }

      if (Objects.isNull(roleRequest.getUsername())) {
        logger.warn("Username and role cannot be null");
        throw new InvalidUserRequestException("Username and role cannot be null");
      }

      String token = authHeader.substring(7);
      ResponseEntity<String> authResponse;

      try {

        List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
        logger.info("Found {} instances of authentication service", instances.size());

        ServiceInstance instance = instances.get(0);
        String authUrl = instance.getUri()+"/auth/"+ roleRequest.getUsername() + "/role";
        HttpHeaders headers = new HttpHeaders();
        headers.set("Authorization", authHeader);
        headers.setContentType(MediaType.APPLICATION_JSON); // Set Content-Type

        HttpEntity<String> requestEntity = new HttpEntity<>(token, headers);
        authResponse = restTemplate.exchange(
            authUrl, HttpMethod.POST, requestEntity, String.class);

           if (!authResponse.getBody().trim().equals(ROLE.ADMIN.toString())) {
          throw new UserNotAdminException("User is not admin");
          }
        return authResponse.getBody();
      } catch (Exception e) {
        throw new UserNotAdminException("User is not admin");

      }

    }

    public String verifyUserRole (VerifyUserRole roleRequest){
    return verifyRole(roleRequest);
    }

  public Boolean verifyUserExists (VerifyUser verifyUserRequest)
      throws AuthorizationHeaderNotFound {

      if (Objects.isNull(verifyUserRequest) || Objects.isNull(verifyUserRequest.getToken())) {
        logger.warn("Username cannot be null");
        throw new InvalidUserRequestException("Username cannot be null");
      }
    return verifyUser(verifyUserRequest);
  }
  }

