package com.eazybooks.user.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;

@Entity
public class Users {

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE)
  @Column(nullable = false)
  private  Long userId;
  private String username;
  private  String salt;
  private String password;
  private  String firstname;
  private String lastname;
  private String email;


  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getSalt() {
    return salt;
  }

  public void setSalt(String salt) {
    this.salt = salt;
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


  public Users(Long userId,String email, String username, String salt, String password, String firstname,
      String lastname) {
    this.userId = userId;
    this.username = username;
    this.salt = salt;
    this.password = password;
    this.firstname = firstname;
    this.lastname = lastname;
    this.email = email;
  }

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public Users() {
  }

  @Override
  public String toString() {
    return "Users{" +
        "userId=" + userId +
        ", username='" + username + '\'' +
        ", salt='" + salt + '\'' +
        ", password='" + password + '\'' +
        ", firstname='" + firstname + '\'' +
        ", lastname='" + lastname + '\'' +
        ", email='" + email + '\'' +
        '}';
  }
}


