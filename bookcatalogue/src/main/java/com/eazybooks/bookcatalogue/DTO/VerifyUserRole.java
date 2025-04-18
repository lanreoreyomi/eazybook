package com.eazybooks.bookcatalogue.DTO;

import org.apache.catalina.Role;
import org.apache.catalina.User;

public class VerifyUserRole {

  String username;
  String header;

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public VerifyUserRole(String username, String header) {
    this.username = username;
    this.header = header;
  }

  public String getHeader() {
    return header;
  }
  public void setHeader(String header) {
    this.header = header;
  }

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    VerifyUserRole that = (VerifyUserRole) o;
    return username.equals(that.username) && header.equals(that.header);
  }

  @Override
  public int hashCode() {
    int result = username.hashCode();
    result = 31 * result + header.hashCode();
    return result;
  }
}
