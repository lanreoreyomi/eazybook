package com.eazybooks.user.controller;

import com.eazybooks.user.DTO.UsersDto;
import com.eazybooks.user.model.CreateAccountRequest;
import com.eazybooks.user.model.User;
import com.eazybooks.user.model.VerifyToken;
import com.eazybooks.user.service.UserService;
import jakarta.servlet.http.HttpServletRequest;
import java.net.URI;
import java.util.List;
import java.util.Optional;
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
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.client.RestTemplate;

@Controller
@RequestMapping("/user")
public class UserController {

  private static final Logger logger = LoggerFactory.getLogger(UserController.class);
  UserService userService;
  private DiscoveryClient discoveryClient;
  RestTemplate restTemplate = new RestTemplate();

  public UserController(UserService userService, DiscoveryClient discoveryClient) {
    this.userService = userService;
    this.discoveryClient = discoveryClient;
  }

  public URI serviceUrl(String serviceId) {
    List<ServiceInstance> list = discoveryClient.getInstances(serviceId);
    if (list != null && list.size() > 0) {

      System.out.println(list.get(0).getUri());
      return list.get(0).getUri();
    }
    return null;
  }

  @GetMapping("/userid/{id}")
  public ResponseEntity<User> getUserById(@PathVariable Long id) {
    logger.info("Received request to get user with ID: {}", id);

    try {
      final User user = userService.findById(id);

      if (user != null) {
        logger.info("Successfully retrieved user with ID: {}", id);
        return ResponseEntity.ok(user); // Return the actual user data
      } else {
        logger.warn("User with ID: {} not found", id);
        return ResponseEntity.notFound().build();
      }

    } catch (Exception e) {
      logger.error("Error while fetching user with ID: {}", id, e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
    }
  }

  @GetMapping("/username/{username}")
  public ResponseEntity<String> getUserByUsername(@PathVariable String username) {
    logger.info("Received request to get user with username: {}", username);

    //TODO: remove Ids from the loggers
    try {
      final User userByUsername = userService.findByUsername(username);

      if (userByUsername != null && userByUsername.getUsername().equals(username)) {
        logger.info("Successfully retrieved user with username: {}", username);
        return ResponseEntity.ok("User found"); // Or return the actual user data
      } else {
        logger.warn("User with username: {} not found", username);
        return ResponseEntity.notFound().build();
      }

    } catch (Exception e) {
      logger.error("Error while fetching user with username: {}", username, e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error fetching user");
    }
  }

  @GetMapping("/{username}")
  public ResponseEntity<Long> getUserIdUsingUsername(@PathVariable String username) {
    logger.info("Received request to get user with username: {}", username);

    //TODO: remove Ids from the loggers
    try {
      final User userByUsername = userService.findByUsername(username);

      if (userByUsername != null && userByUsername.getUsername().equals(username)) {
        logger.info("Successfully retrieved user with username: {}", username);
        logger.info("User found {}", userByUsername.getUserId());
        return ResponseEntity.ok(userByUsername.getUserId()); // Or return the actual user data
      } else {
        logger.warn("User with username: {} not found", username);
        return ResponseEntity.notFound().build();
      }

    } catch (Exception e) {
      logger.error("Error while fetching user with username: {}", username, e);
      return (ResponseEntity<Long>) ResponseEntity.internalServerError();
    }
  }


  @PutMapping("/userid/{id}")
  public ResponseEntity<UsersDto> updateUserById(@PathVariable Long id,
      @RequestBody UsersDto usersDto) {
    logger.info("Received request to get user with id: {}", id);

    //TODO: remove Ids from the loggers
    try {
      final User user = userService.findById(id);

      if (user == null) {
        logger.warn("User with ID: {} not found", id);
        return ResponseEntity.notFound().build();
      }

      User userToUpdate = user;

      // Update user details (excluding password and salt for now)
      userToUpdate.setEmail(usersDto.getEmail());
      userToUpdate.setLastname(usersDto.getLastname());
      userToUpdate.setFirstname(usersDto.getFirstname());

      User updatedUser = userService.updateUser(userToUpdate);
      UsersDto updatedUsersDto = new UsersDto();

      updatedUsersDto.setFirstname(updatedUser.getFirstname());
      updatedUsersDto.setLastname(updatedUser.getLastname());
      updatedUsersDto.setEmail(updatedUser.getEmail());

      logger.info("Successfully updated user with ID: {}", id);

      return ResponseEntity.ok(updatedUsersDto);  // Or return the actual user data
    } catch (Exception e) {
      logger.error("Error while updating user with ID: {}", id, e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body(null); // Or a more specific error response if needed
    }
  }

  @PutMapping("/username/{username}")
  public ResponseEntity<UsersDto> updateUserByUsername(@PathVariable String username,
      @RequestBody UsersDto usersDto) {
    logger.info("Received request to get user with username: {}", username);

     try {
      final User userByUsername = userService.findByUsername(username);

      if (userByUsername == null || !userByUsername.getUsername().equals(username)) {
        logger.warn("User with username: {} not found", username);
        return ResponseEntity.notFound().build();
      }

      User userToUpdate = userByUsername;

      // Update user details (excluding password and salt for now)
      userToUpdate.setEmail(usersDto.getEmail());
      userToUpdate.setLastname(usersDto.getLastname());
      userToUpdate.setFirstname(usersDto.getFirstname());

      User updatedUser = userService.updateUser(userToUpdate);
      logger.info("Successfully updated user with username: {}", username);
      UsersDto updatedUsersDto = new UsersDto();

      updatedUsersDto.setFirstname(updatedUser.getFirstname());
      updatedUsersDto.setLastname(updatedUser.getLastname());
      updatedUsersDto.setEmail(updatedUser.getEmail());

      return ResponseEntity.ok(updatedUsersDto);  // Or return the actual user data


    } catch (Exception e) {
      logger.error("Error while updating user with username: {}", username, e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
          .body(null); // Or a more specific error response if needed
    }
  }

  @PostMapping("/create-account")
  public ResponseEntity<String> signUp( HttpServletRequest request,
      @RequestBody CreateAccountRequest createAccountRequest) {  // Use @RequestBody

    if (createAccountRequest == null) {
      logger.warn("Invalid signup request: User request is null");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    if (userService.findByUsername(createAccountRequest.getUsername()) != null) {
      logger.warn("Username '{}' is already taken", createAccountRequest.getUsername());
      return new ResponseEntity<>("Username already exists", HttpStatus.CONFLICT);
    }

    if (userService.findByEmail(createAccountRequest.getEmail()) != null) {
      logger.warn("Email '{}' is already taken", createAccountRequest.getEmail());
      return new ResponseEntity<>("Email already exists", HttpStatus.CONFLICT);
    }

    final ResponseEntity<String> verified = verifyRequestToken(request);

    logger.info("verified {}", verified);
    try {

      User user = new User();
      user.setUserId(createAccountRequest.getUserId());
      user.setUsername(createAccountRequest.getUsername());
      user.setEmail(createAccountRequest.getEmail());
      user.setFirstname(createAccountRequest.getFirstname());
      user.setLastname(createAccountRequest.getLastname());
      userService.createUser(user);
      return new ResponseEntity<>("User successfully created", HttpStatus.CREATED);
    } catch (Exception e) {
      return new ResponseEntity<>("Error Creating User", HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  private ResponseEntity<String> verifyRequestToken(HttpServletRequest request) {
    String authHeader = request.getHeader("Authorization");

    logger.info("Received request to verify Authorization {}", request.getHeader("Authorization"));

    if (authHeader == null || !authHeader.startsWith("Bearer ")) {
      logger.warn("Authorization header missing or invalid");
      return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
    }

    String token = authHeader.substring(7);

    logger.info("Received request to verify token {}", token);

    try {
      List<ServiceInstance> instances = discoveryClient.getInstances("authentication");
      if (instances.isEmpty()) {
        logger.error("Authentication service not found");
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
        logger.warn("Token validation failed");
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();

      }
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return null;
  }

}
