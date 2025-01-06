package com.eazybooks.bookcatalogue.model;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;
import java.util.Date;

@Entity
public class Checkout {

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE)
  @Column(nullable = false)
  private Long id;
  private Long bookId;
  private Long isbn;
  private String userName;
  private LocalDate dateOfCheckout;
  private LocalDate dateOfReturn;
  private Long checkoutTimes;

  public Boolean getReturned() {
    return isReturned;
  }

  public void setReturned(Boolean returned) {
    isReturned = returned;
  }

  public Boolean getCheckedOut() {
    return isCheckedOut;
  }

  public void setCheckedOut(Boolean checkedOut) {
    isCheckedOut = checkedOut;
  }

  private Boolean isReturned;
  private Boolean isCheckedOut;

  public Long getIsbn() {
    return isbn;
  }

  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public LocalDate getDateOfCheckout() {
    return dateOfCheckout;
  }

  public void setDateOfCheckout(LocalDate dateOfCheckout) {
    this.dateOfCheckout = dateOfCheckout;
  }

  public LocalDate getDateOfReturn() {
    return dateOfReturn;
  }

  public void setDateOfReturn(LocalDate dateOfReturn) {
    this.dateOfReturn = dateOfReturn;
  }

  public Long getCheckoutTimes() {
    return checkoutTimes;
  }

  public void setCheckoutTimes(Long checkoutTimes) {
    this.checkoutTimes = checkoutTimes;
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }
}
