package com.eazybooks.wishlist.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "wishlists")
public class Wishlist {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "created_date")
  private LocalDate localDate;

  @Column(name = "username")
  private String username;

  @Column(name = "book_title")
  private String bookTitle;

  @Column(name = "book_isbn")
  private Long bookIsbn;

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getBookTitle() {
    return bookTitle;
  }

  public void setBookTitle(String bookTitle) {
    this.bookTitle = bookTitle;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public Wishlist(Long id) {
    this.id = id;
  }

  public Wishlist() {
  }

  public LocalDate getLocalDate() {
    return localDate;
  }

  public void setLocalDate(LocalDate localDate) {
    this.localDate = localDate;
  }

  @Override
  public String toString() {
    return "Wishlist{" +
        "id=" + id +
        ", localDate=" + localDate +
        ", username='" + username + '\'' +
        ", bookTitle='" + bookTitle + '\'' +
        ", bookIsbn=" + bookIsbn +
        '}';
  }
}
