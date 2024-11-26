package com.eazybooks.user.model;


public interface UserRepositoryImpl {

  Users createUser(Users user);

  Users updateUser(Users user);

  Users findUserByUserId(Long userId);

  Users findByUsername(String username);
}
