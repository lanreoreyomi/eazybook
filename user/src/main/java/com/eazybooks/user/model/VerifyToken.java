package com.eazybooks.user.model;

public class VerifyToken {

  String token;
  public VerifyToken(String token) {
    this.token = token;
  }
  public String getToken() {
    return token;
  }
  public void setToken(String token) {
    this.token = token;
  }
}
