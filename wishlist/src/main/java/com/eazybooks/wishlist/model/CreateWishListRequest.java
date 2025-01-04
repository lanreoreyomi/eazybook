package com.eazybooks.wishlist.model;

import java.time.LocalDate;

public class CreateWishListRequest
{

  private Long isbn;
  private LocalDate localDate;

  public CreateWishListRequest() {
  }

  public LocalDate getLocalDate() {
    return localDate;
  }

  public void setLocalDate(LocalDate localDate) {
    this.localDate = localDate;
  }

  public Long getIsbn() {
    return isbn;
  }

  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }

  public CreateWishListRequest(Long isbn,
      LocalDate localDate) {
    this.isbn = isbn;
    this.localDate = localDate;
  }

  public CreateWishListRequest(Long isbn) {
    this.isbn = isbn;
  }

  @Override
  public String toString() {
    return "CreateWishListRequest{" +
        ", isbn=" + isbn +
        ", localDate=" + localDate +
        '}';
  }
}

