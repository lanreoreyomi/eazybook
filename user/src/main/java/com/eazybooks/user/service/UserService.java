package com.eazybooks.user.service;


import com.eazybooks.user.model.User;
import com.eazybooks.user.model.UserRepositoryImpl;
import com.eazybooks.user.repository.UserRepository;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Optional;
import org.springframework.stereotype.Service;

@Service
public class UserService implements UserRepositoryImpl {

  private final HashService hashService;
  private final UserRepository userRepository;


  public UserService(UserRepository userRepository, HashService hashService) {
    this.userRepository = userRepository;
    this.hashService = hashService;
  }

  public boolean isUsernameAvailable(String username) {
    return Optional.ofNullable(userRepository.findByUsername(username)).isPresent();
  }

  public boolean isEmailAvailable(String email) {
    return Optional.ofNullable(userRepository.findByEmail(email)).isPresent();
  }

  @Override
  public User createUser(User user) {
    SecureRandom random = new SecureRandom();
    byte[] salt = new byte[16];
    random.nextBytes(salt);
    String encodedSalt = Base64.getEncoder().encodeToString(salt);
    String hashedPassword = hashService.getHashedValue(user.getPassword(), encodedSalt);

    user.setPassword(hashedPassword);

    return userRepository.createUser(user);
  }

  @Override
  public User findUserByUserId(String userId) {
    return userRepository.findUserByUserId(userId);
  }

  @Override
  public User findByUsername(String username) {
    return userRepository.findByUsername(username);
  }


}
