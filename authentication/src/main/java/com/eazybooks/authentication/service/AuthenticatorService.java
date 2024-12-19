package com.eazybooks.authentication.service;

import com.eazybooks.authentication.UserDetails.UserDetailsService;
import com.eazybooks.authentication.model.AuthenticatorImpl;
import com.eazybooks.authentication.model.LoginRequest;
import com.eazybooks.authentication.model.Role;
import com.eazybooks.authentication.model.UserDto.AuthenticationResponse;
import com.eazybooks.authentication.model.UserDto.CreateAccountRequest;
import com.eazybooks.authentication.repository.AuthenticatorRepository;
import jakarta.transaction.Transactional;

import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import com.eazybooks.authentication.model.User;


@Service
@Transactional
public class AuthenticatorService implements AuthenticatorImpl {

  private static final Logger log = LoggerFactory.getLogger(AuthenticatorService.class);
   private final AuthenticatorRepository authenticatorRepository;
  private final AuthenticationManager authenticationManager;
  private final UserDetailsService userDetailService;

  private PasswordEncoder passwordEncoder;
  private JwtService jwtService;

  public AuthenticatorService(
      AuthenticatorRepository authenticatorRepository, AuthenticationManager authenticationManager,
      UserDetailsService userDetailService,
      PasswordEncoder passwordEncoder, JwtService jwtService) {
    this.authenticatorRepository = authenticatorRepository;
    this.authenticationManager = authenticationManager;
    this.userDetailService = userDetailService;
    this.passwordEncoder = passwordEncoder;
    this.jwtService = jwtService;
  }

  @Override
  public Boolean findByUsername(String username) {
    return Optional.ofNullable(authenticatorRepository.findUserByUsername(username)).isPresent();
  }

  @Override
  public Boolean findByEmail(String email) {
    return Optional.ofNullable(authenticatorRepository.findUserByEmail(email)).isPresent();
  }

  @Override
  public AuthenticationResponse createUserAccount(CreateAccountRequest request) {
    User user = new User();
    user.setFirstname(request.getFirstname());
    user.setLastname(request.getLastname());
    user.setEmail(request.getEmail());
    user.setUsername(request.getUsername());
    user.setPassword(passwordEncoder.encode(request.getPassword()));

    user.setRole(Role.USER);

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

  @Override
  public Boolean isTokenValid(String token) {

    try{
      final String username = jwtService.extractUsername(token);
      final UserDetails userDetails = userDetailService.loadUserByUsername(username);
      return jwtService.isTokenValid(token, userDetails);
    } catch (Exception e) {
      log.error("Error while checking if token is valid", e);
    }
  return false;

  }


}
