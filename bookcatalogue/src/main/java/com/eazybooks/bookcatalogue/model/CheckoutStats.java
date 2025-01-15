package com.eazybooks.bookcatalogue.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import org.springframework.boot.context.properties.bind.Name;

@Entity
@Table(name = "checkout_stats")
public class CheckoutStats {

  @Id
  @Column(nullable = false, unique = true)
  @jakarta.persistence.GeneratedValue(strategy = GenerationType.UUID)
   private String id;
  @Column(nullable = false)
  @Name("total_checkout")
   private int totalCheckout;
  @Name("book_isbn")
   private Long bookIsbn;
  @Name("title")
  private String title;
  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public int getTotalCheckout() {
    return totalCheckout;
  }

  public void setTotalCheckout(int totalCheckout) {
    this.totalCheckout = totalCheckout;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public void setId(String id) {
    this.id = id;
  }

  public CheckoutStats() {
  }

  @Override
  public String toString() {
    return "CheckoutStatsRepository{" +
        " totalCheckout=" + totalCheckout+
        ", bookIsbn=" + bookIsbn +
        '}';
  }
}
