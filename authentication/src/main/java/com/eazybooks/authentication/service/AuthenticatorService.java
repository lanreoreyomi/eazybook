package com.eazybooks.authentication.service;

import com.eazybooks.authentication.model.AuthenticatorImpl;
import com.eazybooks.authentication.repository.AuthenticatorRepository;
import jakarta.transaction.Transactional;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Optional;
import org.springframework.stereotype.Service;
import com.eazybooks.authentication.model.Users;


@Service
@Transactional
public class AuthenticatorService implements AuthenticatorImpl {

  private final HashService hashService;
  private final AuthenticatorRepository authenticatorRepository;


  public AuthenticatorService(AuthenticatorRepository authenticatorRepository, HashService hashService) {
    this.authenticatorRepository = authenticatorRepository;
    this.hashService = hashService;
  }


  @Override
  public Users createUser(Users user) {
    SecureRandom random = new SecureRandom();
    byte[] salt = new byte[16];
    random.nextBytes(salt);
    String encodedSalt = Base64.getEncoder().encodeToString(salt);
    String hashedPassword = hashService.getHashedValue(user.getPassword(), encodedSalt);

    user.setPassword(hashedPassword);
    user.setSalt(encodedSalt);
    return authenticatorRepository.save(user);
  }

  @Override
  public Boolean findByUsername(String username) {
    return Optional.ofNullable(authenticatorRepository.findUsersByUsername(username)).isPresent();
  }

  @Override
  public Boolean findByEmail(String email) {
    return Optional.ofNullable(authenticatorRepository.findUsersByEmail(email)).isPresent();
  }


}
