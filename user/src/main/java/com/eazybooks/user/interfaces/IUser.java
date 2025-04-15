package com.eazybooks.user.interfaces;


import com.eazybooks.user.model.User;

public interface IUser {

  User updateUser(User user);
  User findById(Long id);
  User findByEmail(String email);
  User findByUsername(String username);
  User createUser(User user);

}
