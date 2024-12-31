package com.eazybooks.authentication.model.UserDto;

public class AuthenticationResponse {

  private String token;
  private Long userId;

  public AuthenticationResponse(String token, Long userId) {
    this.token = token;
    this.userId = userId;
  }

  public String getToken() {
    return token;
  }

  public void setToken(String token) {
    this.token = token;
  }

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }
}
