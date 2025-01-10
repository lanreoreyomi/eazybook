package com.eazybooks.authentication.model.UserDto;

public class AuthenticationResponse {

  private String token;
  private String userId;

  public AuthenticationResponse(String token, String userId) {
    this.token = token;
    this.userId = userId;
  }

  public String getToken() {
    return token;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public String getUserId() {
    return userId;
  }

  public void setUserId(String userId) {
    this.userId = userId;
  }
}
