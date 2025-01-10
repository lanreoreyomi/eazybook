package com.eazybooks.bookcatalogue.model;

import java.time.LocalDate;

public class CheckoutInfo {
private String nameOfBook;
private Long bookIsbn;
private LocalDate checkoutDate;
private Boolean returnStatus;
private LocalDate expectedReturnDate;

  public CheckoutInfo(String nameOfBook, Long bookIsbn, LocalDate checkoutDate,
      Boolean returnStatus,
      LocalDate expectedReturnDate) {
    this.nameOfBook = nameOfBook;
    this.bookIsbn = bookIsbn;
    this.checkoutDate = checkoutDate;
    this.returnStatus = returnStatus;
    this.expectedReturnDate = expectedReturnDate;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public CheckoutInfo() {
  }

  public String getNameOfBook() {
    return nameOfBook;
  }

  public void setNameOfBook(String nameOfBook) {
    this.nameOfBook = nameOfBook;
  }

  public LocalDate getCheckoutDate() {
    return checkoutDate;
  }

  public void setCheckoutDate(LocalDate checkoutDate) {
    this.checkoutDate = checkoutDate;
  }

  public Boolean getReturnStatus() {
    return returnStatus;
  }

  public void setReturnStatus(Boolean returnStatus) {
    this.returnStatus = returnStatus;
  }

  public LocalDate getExpectedReturnDate() {
    return expectedReturnDate;
  }

  public void setExpectedReturnDate(LocalDate expectedReturnDate) {
    this.expectedReturnDate = expectedReturnDate;
  }


}

