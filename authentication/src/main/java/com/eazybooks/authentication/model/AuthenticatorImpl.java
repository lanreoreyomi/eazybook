package com.eazybooks.authentication.model;


import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;

public interface AuthenticatorImpl {

  AuthenticationResponse registerUser(User user);

  Boolean findByUsername(String username);

  Boolean findByEmail(String username);
}
