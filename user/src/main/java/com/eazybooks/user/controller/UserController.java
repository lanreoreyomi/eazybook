package com.eazybooks.user.controller;

import com.eazybooks.user.DTO.UsersDto;
import com.eazybooks.user.model.CreateAccountRequest;
import com.eazybooks.user.model.User;
import com.eazybooks.user.service.VerificationService;
import com.eazybooks.user.service.UserService;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/user")
public class UserController {

  private static final Logger logger = LoggerFactory.getLogger(UserController.class);
  UserService userService;

  private final VerificationService verificationService;


  public UserController(UserService userService, VerificationService verificationService) {
    this.userService = userService;
     this.verificationService = verificationService;
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
  public ResponseEntity<String> getUserByUsername(@PathVariable String username, HttpServletRequest request) {
    logger.info("Received request to get user with username: {}", username);

    if (username == null) {
      logger.warn(" Username is empty is null");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    final ResponseEntity<Boolean> verified = verificationService.verifyUserToken(request, null);

    if (!Boolean.TRUE.equals(verified.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
    }
    try {
      final User userByUsername = userService.findByUsername(username);

      if (userByUsername != null && userByUsername.getUsername().equals(username)) {
        logger.info("User {} found", username);
        return new ResponseEntity<>(userByUsername.getUsername(), HttpStatus.OK); // Or return the actual user data
      } else {
        logger.warn("User with username: {} not found", username);
        return new ResponseEntity<>("User not found", HttpStatus.NOT_FOUND);
      }

    } catch (Exception e) {
      logger.error("Error while fetching user with username: {}", username, e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error fetching user");
    }
  }

  @GetMapping("/{username}")
  public ResponseEntity<String> getUserIdUsingUsername(@PathVariable String username, HttpServletRequest request) {
    logger.info("Received request to get user with username: {}", username);

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
      return (ResponseEntity<String>) ResponseEntity.internalServerError();
    }
  }


  @PutMapping("/userid/{id}")
  public ResponseEntity<UsersDto> updateUserById(@PathVariable Long id,
      @RequestBody UsersDto usersDto) {
    logger.info("Received request to get user with id: {}", id);

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
  public ResponseEntity<String> updateUserByUsername(@PathVariable String username,
      @RequestBody UsersDto usersDto, HttpServletRequest request) {
    logger.info("Received request to get user with username: {}", username);

    final ResponseEntity<Boolean> verified = verificationService.verifyUserToken(request, null);
    if (!Boolean.TRUE.equals(verified.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
    }

     try {

      final User userByUsername = userService.findByUsername(username);

      if (userByUsername == null || !userByUsername.getUsername().equals(username)) {
        logger.warn("User with username: {} not found", username);
        return ResponseEntity.notFound().build();
      }

      // Update user details (excluding password and salt for now)
       userByUsername.setEmail(usersDto.getEmail());
       userByUsername.setLastname(usersDto.getLastname());
       userByUsername.setFirstname(usersDto.getFirstname());

      User updatedUser = userService.updateUser(userByUsername);
      logger.info("Successfully updated user with username: {}", username);
      UsersDto updatedUsersDto = new UsersDto();

      updatedUsersDto.setFirstname(updatedUser.getFirstname());
      updatedUsersDto.setLastname(updatedUser.getLastname());
      updatedUsersDto.setEmail(updatedUser.getEmail());
      return new ResponseEntity<>("User successfully updated", HttpStatus.OK);  // Or return the actual user data

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

    final ResponseEntity<Boolean> verified = verificationService.verifyUserToken(request, null);

    if (!Boolean.TRUE.equals(verified.getBody())) {
      logger.error("Error validating token");
      return new ResponseEntity<>("Error validating token", HttpStatus.BAD_REQUEST);
    }

    if (userService.findByUsername(createAccountRequest.getUsername()) != null) {
      logger.warn("Username '{}' is already taken", createAccountRequest.getUsername());
      return new ResponseEntity<>("Username already exists", HttpStatus.CONFLICT);
    }

    if (userService.findByEmail(createAccountRequest.getEmail()) != null) {
      logger.warn("Email '{}' is already taken", createAccountRequest.getEmail());
      return new ResponseEntity<>("Email already exists", HttpStatus.CONFLICT);
    }

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

}
