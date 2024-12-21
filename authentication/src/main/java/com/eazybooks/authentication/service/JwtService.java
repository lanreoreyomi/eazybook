package com.eazybooks.authentication.service;

import com.eazybooks.authentication.model.User;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.JwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.security.SignatureException;
import java.util.Date;
import java.util.function.Function;
import javax.crypto.SecretKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;


@Service
public class JwtService {

  private static final Logger log = LoggerFactory.getLogger(JwtService.class);
  private final String SECRET_KEY = "f61ef9704a9371effefe9e5e684dd9e1b4a49d9576c91f64191a9a159cfb765e";

  private Claims extractAllClaims(String token) {

    try {
      return Jwts.parser()
          .verifyWith(getSignInKey())
          .build()
          .parseSignedClaims(token)
          .getPayload();
    } catch (ExpiredJwtException e) {
      log.error("JWT token has expired: {}", e.getMessage());
      // ... handle expired token ...
    } catch (SignatureException e) {
      log.error("Invalid JWT signature: {}", e.getMessage());
      // ... handle invalid signature ...
    } catch (MalformedJwtException e) {
      log.error("Invalid JWT format: {}", e.getMessage());
      // ... handle invalid format ...
    } catch (JwtException e) {
      log.error("Invalid JWT token: {}", e.getMessage());
      // ... handle other JWT-related errors ...
    }
    // ... return null or throw a custom exception ...
    return null;
  }


  public String extractUsername(String token) {
    try {
      return extractClaim(token, Claims::getSubject);
    } catch (Exception e) {
      log.error("Error getting username from token {}", e.getMessage());
    }
  return null;
  }

  public boolean isTokenValid(String token, UserDetails user) {
    String username = extractUsername(token);
    return (username.equals(user.getUsername()) && !isTokenExpired(token));
  }

  private boolean isTokenExpired(String token) {
    return extractExpiration(token).before(new Date());

  }

  private Date extractExpiration(String token) {
    return extractClaim(token, Claims::getExpiration);
  }


  public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {

    Claims claims = extractAllClaims(token);
    return claimsResolver.apply(claims);
  }

  public String generateToken(User user) {

    try {
      String token = Jwts
          .builder()
          .setSubject(user.getUsername())
          .issuedAt(new Date(System.currentTimeMillis()))
          .expiration(new Date(System.currentTimeMillis() + 24 * 60 * 60 * 1000))
          .signWith(getSignInKey())
          .compact();
      return token;
    } catch (Exception e) {
      log.error("Error generating token {}", e.getMessage());
    }
return null;

  }

  private SecretKey getSignInKey() {
    byte[] keyBytes = Decoders.BASE64.decode(SECRET_KEY);
    return Keys.hmacShaKeyFor(keyBytes);

  }


}
