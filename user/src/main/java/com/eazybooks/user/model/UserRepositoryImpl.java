package com.eazybooks.user.model;


public interface UserRepositoryImpl {

  User updateUser(User user);
  User findUserById(Long id);
  User findUserByEmail(String email);
  User findUserByUsername(String username);

}
