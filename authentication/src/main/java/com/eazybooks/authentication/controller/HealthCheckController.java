package com.eazybooks.authentication.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class HealthCheckController {

  @GetMapping("/health")
  public ResponseEntity<String> healthCheck() {
    return new ResponseEntity<>("OK", HttpStatus.OK); // 200 OK
  }
}
