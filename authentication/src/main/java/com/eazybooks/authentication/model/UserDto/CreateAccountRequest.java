package com.eazybooks.authentication.model.UserDto;

import java.util.Objects;

public class CreateAccountRequest {
  private String username;
  private String password;
  private  String firstname;
  private String lastname;
  private String email;

  public CreateAccountRequest(String username, String password, String firstname, String lastname,
      String email) {
    this.username = username;
    this.password = password;
    this.firstname = firstname;
    this.lastname = lastname;
    this.email = email;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public String getFirstname() {
    return firstname;
  }

  public void setFirstname(String firstname) {
    this.firstname = firstname;
  }

  public String getLastname() {
    return lastname;
  }

  public void setLastname(String lastname) {
    this.lastname = lastname;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  @Override
  public String toString() {
    return "CreateAccountRequest{" +
        "username='" + username + '\'' +
        ", password='" + password + '\'' +
        ", firstname='" + firstname + '\'' +
        ", lastname='" + lastname + '\'' +
        ", email='" + email + '\'' +
        '}';
  }

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    CreateAccountRequest that = (CreateAccountRequest) o;
    return Objects.equals(username, that.username) && Objects.equals(password,
        that.password) && Objects.equals(firstname, that.firstname)
        && Objects.equals(lastname, that.lastname) && Objects.equals(email,
        that.email);
  }

  @Override
  public int hashCode() {
    int result = Objects.hashCode(username);
    result = 31 * result + Objects.hashCode(password);
    result = 31 * result + Objects.hashCode(firstname);
    result = 31 * result + Objects.hashCode(lastname);
    result = 31 * result + Objects.hashCode(email);
    return result;
  }
}