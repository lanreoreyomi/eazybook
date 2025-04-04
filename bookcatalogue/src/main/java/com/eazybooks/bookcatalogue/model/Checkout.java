package com.eazybooks.bookcatalogue.model;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;

@Entity
@Table(name = "checkout")
public class Checkout {


  @Id
  @Column(nullable = false, unique = true)
  @jakarta.persistence.GeneratedValue(strategy = GenerationType.UUID)
  private String id;
  private Long isbn;
  private String checkedOutBy;
  private LocalDate dateOfCheckout;
  private LocalDate expectedReturnDate;

  public Boolean getReturned() {
    return isReturned;
  }

  public void setReturned(Boolean returned) {
    isReturned = returned;
  }

  private Boolean isReturned;

  public Long getIsbn() {
    return isbn;
  }

  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }

  public String getCheckedOutBy() {
    return checkedOutBy;
  }

  public void setCheckedOutBy(String checkedOutBy) {
    this.checkedOutBy = checkedOutBy;
  }

  public LocalDate getDateOfCheckout() {
    return dateOfCheckout;
  }

  public void setDateOfCheckout(LocalDate dateOfCheckout) {
    this.dateOfCheckout = dateOfCheckout;
  }

  public LocalDate getExpectedReturnDate() {
    return expectedReturnDate;
  }

  public void setExpectedReturnDate(LocalDate expectedReturnDate) {
    this.expectedReturnDate = expectedReturnDate;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  @Override
  public String toString() {
    return "Checkout{" +
        "id=" + id +
        ", isbn=" + isbn +
        ", checkedOutBy='" + checkedOutBy + '\'' +
        ", dateOfCheckout=" + dateOfCheckout +
        ", dateOfReturn=" + expectedReturnDate +
         ", isReturned=" + isReturned +
        '}';
  }
}
