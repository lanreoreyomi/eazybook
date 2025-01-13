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
  @Column(nullable = false, unique = true)
  @jakarta.persistence.GeneratedValue(strategy = GenerationType.UUID)
  private String id;

  @Column(name = "token")
  private String token;
  @Column(name = "username")
  private String username;
  @Column(name = "is_logged_out")
  private Boolean isLoggedOut;

  public void setId(String id) {
    this.id = id;
  }


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
