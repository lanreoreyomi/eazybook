package com.eazybooks.user.model;


public interface UserRepositoryImpl {

  User updateUser(User user);
  User findById(Long id);
  User findByEmail(String email);
  User findByUsername(String username);
  User createUser(User user);

}
