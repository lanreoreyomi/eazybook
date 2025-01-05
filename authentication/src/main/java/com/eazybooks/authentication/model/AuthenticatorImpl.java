package com.eazybooks.authentication.model;


import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;

public interface AuthenticatorImpl {

  AuthenticationResponse createUserAccount(CreateAccountRequest user);

  Boolean findByUsername(String username);
  Boolean findByEmail(String username);
  Boolean isTokenValid(String token);
  String findUserByRole(String username);
}
