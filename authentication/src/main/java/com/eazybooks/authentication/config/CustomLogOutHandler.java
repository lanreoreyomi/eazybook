package com.eazybooks.authentication.config;

import com.eazybooks.authentication.model.Token;
import com.eazybooks.authentication.repository.TokenRepository;
import com.eazybooks.authentication.service.AuthenticatorService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutHandler;
import org.springframework.stereotype.Component;

@Component
public class CustomLogOutHandler implements LogoutHandler {

  private static final Logger log = LoggerFactory.getLogger(CustomLogOutHandler.class);

  private TokenRepository tokenRepository;

  public CustomLogOutHandler(TokenRepository tokenRepository) {
    this.tokenRepository = tokenRepository;
  }

  @Override
  public void logout(HttpServletRequest request, HttpServletResponse response,
      Authentication authentication) {

    log.info("Logout request received");


    String authHeader = request.getHeader("Authorization");
    if (authHeader == null || !authHeader.startsWith("Bearer ")) {

    }
    String token = null;
    try{
      token  = authHeader.substring(7);
    } catch (Exception e) {
      log.error("User not signed in {}", e.getMessage());
    }

    if (token != null) {

      final Token userToken = tokenRepository.findByToken(token);

      if (userToken == null) {
        return;
      }

      userToken.setLoggedOut(true);
      tokenRepository.save(userToken);
    }


  }
}
