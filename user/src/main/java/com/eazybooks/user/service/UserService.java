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

  @Override
  public User updateUser(User user) {
   return userRepository.save(user);
  }


  @Override
  public User findByEmail(String email) {

   return Optional.ofNullable(userRepository.findByEmail(email)).get().orElse(null);

  }

  @Override
  public User findByUsername(String username) {
    return Optional.ofNullable(userRepository.findByUsername(username)).get().orElse(null);
  }

  @Override
  public User createUser(User user) {
    return userRepository.save(user);
  }

  public User findById(Long id) {
  return Optional.ofNullable(userRepository.findById(id)).get().orElse(null);
  }
}
