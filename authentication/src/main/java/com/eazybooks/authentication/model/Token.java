package com.eazybooks.authentication.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

@Entity
@Table(name = "Token")
public class Token {

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE)
  @Column(nullable = false)
  private Long id;

  public void setId(Long id) {
    this.id = id;
  }

  private String token;
  private String username;
  private Boolean isLoggedOut;

  public Token() {
  }

  public String getToken() {
    return token;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public Boolean getLoggedOut() {
    return isLoggedOut;
  }

  public void setLoggedOut(Boolean loggedOut) {
    isLoggedOut = loggedOut;
  }
}
