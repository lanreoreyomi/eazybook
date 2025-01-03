package com.eazybooks.wishlist.model;

import java.time.LocalDate;

public class CreateWishListRequest
{

  private Long bookIsbn;
  private LocalDate localDate;

  public CreateWishListRequest() {
  }

  public LocalDate getLocalDate() {
    return localDate;
  }

  public void setLocalDate(LocalDate localDate) {
    this.localDate = localDate;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public CreateWishListRequest(Long bookIsbn, String userName,
      LocalDate localDate) {
    this.bookIsbn = bookIsbn;
    this.localDate = localDate;
  }

  @Override
  public String toString() {
    return "CreateWishListRequest{" +
        ", bookIsbn=" + bookIsbn +
        ", localDate=" + localDate +
        '}';
  }
}

