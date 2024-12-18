package com.eazybooks.user.service;


import com.eazybooks.user.model.Users;
import com.eazybooks.user.model.UserRepositoryImpl;
import com.eazybooks.user.repository.UserRepository;
import jakarta.transaction.Transactional;
import java.security.SecureRandom;
import java.util.Base64;
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
    return Optional.ofNullable(userRepository.findUsersByUsername(username)).isPresent();
  }

  public boolean isEmailAvailable(String email) {
    return Optional.ofNullable(userRepository.findUsersByEmail(email)).isPresent();
  }


  @Override
  public Users updateUser(Users user) {
   return userRepository.save(user);
  }

}
