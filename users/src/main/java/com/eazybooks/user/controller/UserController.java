package com.eazybooks.user.controller;

import com.eazybooks.user.DTO.UsersDto;
import com.eazybooks.user.model.Users;
import com.eazybooks.user.service.UserService;
import java.net.URI;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/users")
public class UserController {

  private static final Logger logger = LoggerFactory.getLogger(UserController.class);
  UserService userService;


  public UserController(UserService userService) {
    this.userService = userService;

  }
  @Autowired
  private DiscoveryClient discoveryClient;

  public URI serviceUrl(String serviceId) {
    List<ServiceInstance> list = discoveryClient.getInstances(serviceId);
    if (list != null && list.size() > 0 ) {

      System.out.println(list.get(0).getUri());
      return list.get(0).getUri();
    }
    return null;
  }
  @GetMapping("userid/{id}")
  public ResponseEntity<Users> getUserById(@PathVariable Long id) {
    logger.info("Received request to get user with ID: {}", id);

    try {
      final Users user = userService.findUserByUserId(id);

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
      final Users user = userService.findByUsername(username);

      if (user != null) {
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

  @PutMapping("/userid/{id}")
  public ResponseEntity<UsersDto> updateUserById(@PathVariable Long id, @RequestBody UsersDto usersDto) {
    logger.info("Received request to get user with id: {}", id);

    //TODO: remove Ids from the loggers
    try {
      Optional<Users> optionalUser = Optional.ofNullable(userService.findUserByUserId(id));

      if (optionalUser.isEmpty()) {
        logger.warn("User with ID: {} not found", id);
        return ResponseEntity.notFound().build();
      }

      Users userToUpdate = optionalUser.get();

      // Update user details (excluding password and salt for now)
       userToUpdate.setEmail(usersDto.getEmail());
      userToUpdate.setLastname(usersDto.getLastname());
      userToUpdate.setFirstname(usersDto.getFirstname());

      Users updatedUser = userService.updateUser(userToUpdate);
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
  public ResponseEntity<UsersDto> updateUserByUsername(@PathVariable String username, @RequestBody UsersDto usersDto) {
    logger.info("Received request to get user with username: {}", username);

    //TODO: remove Ids from the loggers
    try {
      Optional<Users> optionalUser = Optional.ofNullable(userService.findByUsername(username));

      if (optionalUser.isEmpty()) {
        logger.warn("User with username: {} not found", username);
        return ResponseEntity.notFound().build();
      }

      Users userToUpdate = optionalUser.get();

      // Update user details (excluding password and salt for now)
      userToUpdate.setEmail(usersDto.getEmail());
      userToUpdate.setLastname(usersDto.getLastname());
      userToUpdate.setFirstname(usersDto.getFirstname());

      Users updatedUser = userService.updateUser(userToUpdate);
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
}
