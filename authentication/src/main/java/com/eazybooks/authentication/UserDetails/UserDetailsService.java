package com.eazybooks.authentication.UserDetails;

import com.eazybooks.authentication.repository.AuthenticatorRepository;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class UserDetailsService implements
    org.springframework.security.core.userdetails.UserDetailsService {

  private final AuthenticatorRepository authenticatorRepository;

  public UserDetailsService(AuthenticatorRepository authenticatorRepository) {
    this.authenticatorRepository = authenticatorRepository;
  }

  @Override
  public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
    return authenticatorRepository.findUserByUsername(username);
  }
}
