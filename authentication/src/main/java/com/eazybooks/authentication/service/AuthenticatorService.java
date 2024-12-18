package com.eazybooks.authentication.service;

import com.eazybooks.authentication.model.AuthenticatorImpl;
import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.repository.AuthenticatorRepository;
import jakarta.transaction.Transactional;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import com.eazybooks.authentication.model.User;


@Service
@Transactional
public class AuthenticatorService implements AuthenticatorImpl {

  private static final Logger log = LoggerFactory.getLogger(AuthenticatorService.class);
  private final HashService hashService;
  private final AuthenticatorRepository authenticatorRepository;
  private final AuthenticationManager authenticationManager;

  private AuthenticatorRepository userRepository;
  private PasswordEncoder passwordEncoder;
  private JwtService jwtService;

  public AuthenticatorService(HashService hashService,
      AuthenticatorRepository authenticatorRepository, AuthenticationManager authenticationManager,
      AuthenticatorRepository userRepository,
      PasswordEncoder passwordEncoder, JwtService jwtService) {
    this.hashService = hashService;
    this.authenticatorRepository = authenticatorRepository;
    this.authenticationManager = authenticationManager;
    this.userRepository = userRepository;
    this.passwordEncoder = passwordEncoder;
    this.jwtService = jwtService;
  }

//  @Override
//  public User createUser(User user) {
//    SecureRandom random = new SecureRandom();
//    byte[] salt = new byte[16];
//    random.nextBytes(salt);
//    String encodedSalt = Base64.getEncoder().encodeToString(salt);
//    String hashedPassword = hashService.getHashedValue(user.getPassword(), encodedSalt);
//
//    user.setPassword(hashedPassword);
//    user.setSalt(encodedSalt);
//    return authenticatorRepository.save(user);
//  }

  @Override
  public Boolean findByUsername(String username) {
    return Optional.ofNullable(authenticatorRepository.findUserByUsername(username)).isPresent();
  }

  @Override
  public Boolean findByEmail(String email) {
    return Optional.ofNullable(authenticatorRepository.findUserByEmail(email)).isPresent();
  }

  @Override
  public AuthenticationResponse registerUser(User request) {
    User user = new User();
    user.setFirstname(request.getFirstname());
    user.setLastname(request.getLastname());
    user.setEmail(request.getEmail());
    user.setUsername(request.getUsername());
    user.setPassword(passwordEncoder.encode(request.getPassword()));
    user.setRole(request.getRole());

    user = authenticatorRepository.save(user);

    String token = jwtService.generateToken(user);

    log.info("User registered successfully with username: {}", user.getUsername());

    return new AuthenticationResponse(token);
  }

  public AuthenticationResponse authenticate(LoginRequest request) {

    log.info("recieved authentication request inside Service{}", request);
    authenticationManager
        .authenticate(new UsernamePasswordAuthenticationToken(
            request.getUsername(), request.getPassword())
        );

    User user = authenticatorRepository.findUserByUsername(request.getUsername());
    String token = jwtService.generateToken(user);
    return new AuthenticationResponse(token);
  }
}
