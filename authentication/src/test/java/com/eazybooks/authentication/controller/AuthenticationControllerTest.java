package com.eazybooks.authentication.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.authentication.config.SERVICES;
import com.eazybooks.authentication.model.User;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.service.AuthenticatorService;
import com.eazybooks.authentication.service.JwtService;
import java.net.URI;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

@SpringBootTest
@ExtendWith(MockitoExtension.class)
public class AuthenticationControllerTest {
  @Mock
  private AuthenticatorService authenticatorService;

  @Mock
  private DiscoveryClient discoveryClient;

  @Mock
  private JwtService jwtService;

  @InjectMocks
  private AuthenticationController authenticationController;

  private final RestTemplate restTemplate = new RestTemplate();

  private static User createUser(CreateAccountRequest request) {
    User user = new User();
    user.setUsername(request.getUsername());
    user.setPassword(request.getPassword());
    user.setEmail(request.getEmail());
    user.setUserId(request.getUserId());
    user.setFirstname(request.getFirstname());
    user.setLastname(request.getLastname());
    return user;
  }

  private static CreateAccountRequest getCreateAccountRequest(UUID uuid,  String userId ) {
    CreateAccountRequest request = new CreateAccountRequest(
        userId,
        "test",
        "password",
        "test_fname",
        "test_lname",
        "test@email.com"
    );
    return request;
  }
}
