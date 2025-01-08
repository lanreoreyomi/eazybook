package com.eazybooks.bookcatalogue.model;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;
import org.springframework.boot.context.properties.bind.Name;

@Entity
@Table(name = "checkout_items")
public class CheckoutItems {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Name("book_isbn")
    private Long bookIsbn;
    @Name("username")
    private String username;

  public CheckoutItems(String username, Long bookIsbn) {
    this.username = username;
    this.bookIsbn = bookIsbn;
  }

  public void setBookId(Long id) {
    this.id = id;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public Long getId() {
    return id;
  }

  @Override
  public String toString() {
    return "CheckoutItems{" +
        "id=" + id +
        ", bookIsbn=" + bookIsbn +
        ", username='" + username + '\'' +
        '}';
  }

  public CheckoutItems() {
  }
}
