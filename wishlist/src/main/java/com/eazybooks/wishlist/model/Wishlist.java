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
  @Column(nullable = false, unique = true)
  @jakarta.persistence.GeneratedValue(strategy = GenerationType.UUID)
  private String id;

  @Column(name = "created_date")
  private LocalDate localDate;

  @Column(name = "username")
  private String username;

  @Column(name = "book_title")
  private String bookTitle;

  @Column(name = "book_isbn")
  private Long isbn;

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

  public Long getIsbn() {
    return isbn;
  }

  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }

  public Wishlist(String id) {
    this.id = id;
  }

  public Wishlist() {
  }

  public String getLocalDate() {
    return String.valueOf(localDate);
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
        ", isbn=" + isbn +
        '}';
  }
}
