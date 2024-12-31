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

  @Column(name = "user_id")
  private Long userId;

  @Column(name = "book_catalogue_id")
  private Long bookCatalogueId;

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  @Column(name = "username")
  private String username;

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

  @Column(name = "book_title")
  private String bookTitle;

  @Column(name = "book_isbn")
  private Long bookIsbn;

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

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public Long getBookCatalogueId() {
    return bookCatalogueId;
  }

  public void setBookCatalogueId(Long bookCatalogueId) {
    this.bookCatalogueId = bookCatalogueId;
  }
}
