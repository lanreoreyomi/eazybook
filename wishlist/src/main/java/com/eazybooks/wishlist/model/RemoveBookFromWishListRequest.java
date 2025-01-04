package com.eazybooks.wishlist.model;

public class RemoveBookFromWishListRequest
{
  private Long isbn;

  public RemoveBookFromWishListRequest() {
  }
  public Long getIsbn() {
    return isbn;
  }
  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }

  public RemoveBookFromWishListRequest( Long isbn) {
    this.isbn = isbn;
  }
  @Override
  public String toString() {
    return "RemoveBookFromWishListRequest{" +
        "isbn=" + isbn + '\'' +
        '}';
  }
}

