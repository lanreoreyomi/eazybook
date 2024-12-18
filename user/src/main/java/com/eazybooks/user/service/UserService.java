package com.eazybooks.user.service;


import com.eazybooks.user.model.User;
import com.eazybooks.user.model.UserRepositoryImpl;
import com.eazybooks.user.repository.UserRepository;
import jakarta.transaction.Transactional;
import java.util.Optional;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class UserService implements UserRepositoryImpl {

  private final UserRepository userRepository;

  public UserService(UserRepository userRepository) {
    this.userRepository = userRepository;
  }

  public boolean isUsernameAvailable(String username) {
    return Optional.ofNullable(userRepository.findUserByUsername(username)).isPresent();
  }

  public boolean isEmailAvailable(String email) {
    return Optional.ofNullable(userRepository.findUserByEmail(email)).isPresent();
  }


  @Override
  public User updateUser(User user) {
   return userRepository.save(user);
  }

  @Override
  public User findUserById(Long id) {
    return null;
  }

  @Override
  public User findUserByEmail(String email) {
    return Optional.ofNullable(userRepository.findUserByEmail(email)).get();
  }

  @Override
  public User findUserByUsername(String username) {
    return Optional.ofNullable(userRepository.findUserByUsername(username)).get();
  }

  public User findUserByUserId(Long id) {
    return Optional.ofNullable(userRepository.findUserByUserId(id)).get();
  }
}
