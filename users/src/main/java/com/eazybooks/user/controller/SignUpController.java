package com.eazybooks.user.controller;

import com.eazybooks.user.model.Users;
import com.eazybooks.user.service.UserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/signup")
public class SignUpController {

  private static final Logger logger = LoggerFactory.getLogger(SignUpController.class);
  UserService userService;


  public SignUpController(UserService userService) {
    this.userService = userService;

  }



  @PostMapping()
  public ResponseEntity<String> signUp(@RequestBody Users user) {  // Use @RequestBody

    if (user == null) {
      logger.warn("Invalid signup request: User object is null");
      return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
    }

    if (userService.isUsernameAvailable(user.getUsername())) {
      logger.warn("Username '{}' is already taken", user.getUsername());
      return new ResponseEntity<>("Username already exists", HttpStatus.CONFLICT);
    }

    if (userService.isEmailAvailable(user.getEmail())) {
      logger.warn("Email '{}' is already taken", user.getEmail());
      return new ResponseEntity<>("Email already exists", HttpStatus.CONFLICT);
    }

    userService.createUser(user);
    logger.info("User '{}' successfully created", user.getUsername());

    return new ResponseEntity<>("User successfully created", HttpStatus.CREATED);
  }
}