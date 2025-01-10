package com.eazybooks.user.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.Collection;
import java.util.List;

@Entity
@Table(name = "users")
public class User {


  @Id
 //  @GeneratedValue(strategy = GenerationType.SEQUENCE)
  @Column(nullable = false)
  private  String userId;
  private String username;
  private  String firstname;
  private String lastname;
  private String email;

  public String getUserId() {
    return userId;
  }

  public void setUserId(String userId) {
    this.userId = userId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
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


  public User(Long userId,String email, String username, String firstname,
      String lastname) {
    this.username = username;
      this.firstname = firstname;
    this.lastname = lastname;
    this.email = email;
  }

  public User() {
  }

  @Override
  public String toString() {
    return "User{" +
        ", username='" + username + '\'' +
           ", firstname='" + firstname + '\'' +
        ", lastname='" + lastname + '\'' +
        ", email='" + email + '\'' +
        '}';
  }
}


