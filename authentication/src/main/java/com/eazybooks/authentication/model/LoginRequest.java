package com.eazybooks.authentication.model;


public class LoginRequest {

  private String username;
  private String password;
  public String getUsername() {
    return username;
  }

  public LoginRequest() {
  }

  public void setUsername(String username) {
    this.username = username.toLowerCase();
    }
    public String getPassword() {
    return password;
    }
    public void setPassword(String password) {
    this.password = password;
    }

  public LoginRequest(String username, String password) {
    this.username = username;
    this.password = password;
  }

  @Override
  public String toString() {
    return "LoginRequest{" +
        "username='" + username + '\'' +
        ", password='" + password + '\'' +
        '}';
  }
}
