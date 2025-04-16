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

  public String getCheckoutDate() {
    return String.valueOf(checkoutDate);
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

  public String getExpectedReturnDate() {
    return String.valueOf(expectedReturnDate);
  }

  public void setExpectedReturnDate(LocalDate expectedReturnDate) {
    this.expectedReturnDate = expectedReturnDate;
  }

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    CheckoutInfo that = (CheckoutInfo) o;
    return nameOfBook.equals(that.nameOfBook) && bookIsbn.equals(that.bookIsbn)
        && checkoutDate.equals(that.checkoutDate) && returnStatus.equals(that.returnStatus)
        && expectedReturnDate.equals(that.expectedReturnDate);
  }

  @Override
  public int hashCode() {
    int result = nameOfBook.hashCode();
    result = 31 * result + bookIsbn.hashCode();
    result = 31 * result + checkoutDate.hashCode();
    result = 31 * result + returnStatus.hashCode();
    result = 31 * result + expectedReturnDate.hashCode();
    return result;
  }
}

