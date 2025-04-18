package com.eazybooks.bookcatalogue.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
 import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
 import org.springframework.boot.context.properties.bind.Name;

@Entity
@Table(name = "checkout_items")
public class CheckoutItems {


  @Id
  @Column(nullable = false, unique = true)
  @jakarta.persistence.GeneratedValue(strategy = GenerationType.UUID)
    private String id;
    @Name("book_isbn")
    private Long bookIsbn;
    @Name("username")
    private String username;

  public CheckoutItems(String username, Long bookIsbn) {
    this.username = username;
    this.bookIsbn = bookIsbn;
  }

  public void setBookId(String id) {
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

  public String getId() {
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

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    CheckoutItems that = (CheckoutItems) o;
    return id.equals(that.id) && bookIsbn.equals(that.bookIsbn) && username.equals(that.username);
  }

  @Override
  public int hashCode() {
    int result = id.hashCode();
    result = 31 * result + bookIsbn.hashCode();
    result = 31 * result + username.hashCode();
    return result;
  }
}
