package com.eazybooks.authentication.model;


public interface AuthenticatorImpl {

  Users createUser(Users user);

  Boolean findByUsername(String username);

  Boolean findByEmail(String username);
}
