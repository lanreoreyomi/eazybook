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

  private final HashService hashService;
  private final UserRepository userRepository;


  public UserService(UserRepository userRepository, HashService hashService) {
    this.userRepository = userRepository;
    this.hashService = hashService;
  }

  public boolean isUsernameAvailable(String username) {
    return Optional.ofNullable(userRepository.getUsersByUsername(username)).isPresent();
  }

  public boolean isEmailAvailable(String email) {
    return Optional.ofNullable(userRepository.getUsersByEmail(email)).isPresent();
  }

  @Override
  public void createUser(Users user) {
    SecureRandom random = new SecureRandom();
    byte[] salt = new byte[16];
    random.nextBytes(salt);
    String encodedSalt = Base64.getEncoder().encodeToString(salt);
    String hashedPassword = hashService.getHashedValue(user.getPassword(), encodedSalt);

    user.setPassword(hashedPassword);
    user.setSalt(encodedSalt);

    userRepository.save(user);
  }

  @Override
  public Users updateUser(Users user) {
   return userRepository.save(user);
  }

  @Override
  public Users findUserByUserId(Long userId) {
    return userRepository.getUsersByUserId(userId);
  }

  @Override
  public Users findByUsername(String username) {
    return userRepository.getUsersByUsername(username);
  }


}