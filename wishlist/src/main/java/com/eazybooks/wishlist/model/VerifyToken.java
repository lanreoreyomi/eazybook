package com.eazybooks.wishlist.model;

public class VerifyToken {

  String token;
  String username;

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

  public VerifyToken(String token, String username) {
    this.token = token;
    this.username = username;
  }

}