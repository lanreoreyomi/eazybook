package com.eazybooks.wishlist.model;

public class BookCatalogue {

  private Long bookId;
  private String title;
  private String author;
  private Long isbn;

  public BookCatalogue(Long  isbn) {
    this.bookId = bookId;
  }

  public Long getBookId() {
    return bookId;
  }

  public void setBookId(Long bookId) {
    this.bookId = bookId;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public String getAuthor() {
    return author;
  }

  public void setAuthor(String author) {
    this.author = author;
  }

  public Long getIsbn() {
    return isbn;
  }

  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }
}
