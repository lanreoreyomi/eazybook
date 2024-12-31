package com.eazybooks.wishlist.model;

import java.time.LocalDate;

public class CreateWishListRequest
{

  private Long userId;
  private Long bookCatalogueId;
  private Long bookIsbn;
  private String bookTitle;
  private String userName;
  private LocalDate localDate;

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public CreateWishListRequest() {
  }

  public Long getUserId() {
    return userId;
  }

  public LocalDate getLocalDate() {
    return localDate;
  }

  public void setLocalDate(LocalDate localDate) {
    this.localDate = localDate;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public Long getBookCatalogueId() {
    return bookCatalogueId;
  }

  public void setBookCatalogueId(Long bookCatalogueId) {
    this.bookCatalogueId = bookCatalogueId;
  }

  public Long getBookIsbn() {
    return bookIsbn;
  }

  public void setBookIsbn(Long bookIsbn) {
    this.bookIsbn = bookIsbn;
  }

  public String getBookTitle() {
    return bookTitle;
  }

  public void setBookTitle(String bookTitle) {
    this.bookTitle = bookTitle;
  }

  public CreateWishListRequest(Long userId, Long bookCatalogueId, Long bookIsbn, String bookTitle,
      LocalDate localDate) {
    this.userId = userId;
    this.bookCatalogueId = bookCatalogueId;
    this.bookIsbn = bookIsbn;
    this.bookTitle = bookTitle;
    this.localDate = localDate;
  }

  @Override
  public String toString() {
    return "CreateWishListRequest{" +
        "userId=" + userId +
        ", bookCatalogueId=" + bookCatalogueId +
        ", bookIsbn=" + bookIsbn +
        ", bookUsername=" + userName +
        ", bookTitle=" + bookTitle +
        ", localDate=" + localDate +
        '}';
  }
}

