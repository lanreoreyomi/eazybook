package com.eazybooks.bookcatalogue.model;

import java.util.Objects;

public class MaxCheckoutResponse {

  private int maxCheckout;
  private String bookIsbn;

  public MaxCheckoutResponse(String bookIsbn, int maxCheckout) {
    this.bookIsbn = bookIsbn;
    this.maxCheckout = maxCheckout;
  }

  public int getMaxCheckout() {
    return maxCheckout;
  }

  public void setMaxCheckout(int maxCheckout) {
    this.maxCheckout = maxCheckout;
  }

  public String getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(String bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    MaxCheckoutResponse that = (MaxCheckoutResponse) o;
    return maxCheckout == that.maxCheckout && Objects.equals(bookIsbn, that.bookIsbn);
  }

  @Override
  public int hashCode() {
    int result = maxCheckout;
    result = 31 * result + Objects.hashCode(bookIsbn);
    return result;
  }

  @Override
  public String toString() {
    return "MaxCheckoutResponse{" +
        "maxCheckout=" + maxCheckout +
        ", bookIsbn='" + bookIsbn + '\'' +
        '}';
  }
}
