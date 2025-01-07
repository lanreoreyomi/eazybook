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
  @Column(nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
   private Long id;
  @Column(nullable = false)
  @Name("total_checkout")
   private Long totalCheckout;
  @Name("book_isbn")
   private Long bookIsbn;

  public CheckoutStats(Long totalCheckout, Long bookIsbn) {
    this.totalCheckout = totalCheckout;
    this.bookIsbn = bookIsbn;
  }

  public Long getId() {
    return id;
  }

  public Long getTotalCheckouts() {
    return totalCheckout;
  }

  public void setTotalCheckout(Long totalCheckout) {
    this.totalCheckout = totalCheckout;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public void setId(Long id) {
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
