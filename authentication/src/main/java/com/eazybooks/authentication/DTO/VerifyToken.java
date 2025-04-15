package com.eazybooks.authentication.DTO;

public class VerifyToken {

  String token;
  String username;

  public VerifyToken(String token, String username) {
    this.token = token;
    this.username = username;
  }

  public String getToken() {
    return token;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public VerifyToken() {
  }
}
