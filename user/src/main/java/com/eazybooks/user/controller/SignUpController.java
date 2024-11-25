package com.eazybooks.user.controller;

import com.eazybooks.user.model.User;
import com.eazybooks.user.service.UserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/signup")
public class SignUpController {

  private static final Logger log = LoggerFactory.getLogger(SignUpController.class);
  UserService userService;


  public SignUpController(UserService userService) {
    this.userService = userService;

  }

  @GetMapping
  public String userSignUp() {
    return "signup";

  }

  @PostMapping()
  public String signupUser(@ModelAttribute User user) {
    String signupError = null;

    if (!userService.isUsernameAvailable(user.getUsername())) {
      signupError = "The username already exists.";
    }

    if (!userService.isEmailAvailable(user.getEmail())) {
      signupError = "The email already exists.";
    }

    if (signupError == null) {
      int rowsAdded = userService.createUser(user).getUserId();
      if (rowsAdded < 0) {
        signupError = "There was an error signing you up. Please try again.";
      }
    }
return signupError;
  }
}