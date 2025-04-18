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

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    VerifyToken that = (VerifyToken) o;
    return token.equals(that.token) && username.equals(that.username);
  }

  @Override
  public int hashCode() {
    int result = token.hashCode();
    result = 31 * result + username.hashCode();
    return result;
  }

  @Override
  public String toString() {
    return "VerifyToken{" +
        "token='" + token + '\'' +
        ", username='" + username + '\'' +
        '}';
  }
}
