package com.eazybooks.wishlist.model;

public class RemoveBookFromWishListRequest
{
  private Long bookIsbn;

  public RemoveBookFromWishListRequest() {
  }
  public Long getBookIsbn() {
    return bookIsbn;
  }
  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public RemoveBookFromWishListRequest( Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }
  @Override
  public String toString() {
    return "RemoveBookFromWishListRequest{" +
        "bookIsbn=" + bookIsbn + '\'' +
        '}';
  }
}

