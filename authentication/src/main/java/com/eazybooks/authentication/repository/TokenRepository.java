package com.eazybooks.authentication.repository;

import com.eazybooks.authentication.model.Token;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TokenRepository extends JpaRepository<Token, Long> {

  Token save(Token token);
  Token findByToken(String token);
  Token findByUsername(String username);
  List<Token> findAllByUsername(String username);

}
