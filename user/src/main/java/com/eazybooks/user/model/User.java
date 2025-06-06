package com.eazybooks.user.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;


@Entity
@Table(name = "users")
public class User {


  @Id
  @Column(nullable = false, unique = true)
//  @jakarta.persistence.GeneratedValue(strategy = GenerationType.UUID)
  private String userId;
  @Column(name = "username")
  private String username;
  @Column(name = "first_name")
  private  String firstname;
  @Column(name = "last_name")
  private String lastname;
  @Column(name = "email")
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


