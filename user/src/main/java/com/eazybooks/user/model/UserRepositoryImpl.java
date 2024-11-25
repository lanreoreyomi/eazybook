package com.eazybooks.user.model;

public interface UserRepositoryImpl {

  User createUser(User user);

  User findUserByUserId(String userId);

  User findByUsername(String username);
}
