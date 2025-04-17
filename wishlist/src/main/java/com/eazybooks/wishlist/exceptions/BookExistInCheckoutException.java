package com.eazybooks.wishlist.exceptions;

public class BookExistInCheckoutException extends RuntimeException {

  public BookExistInCheckoutException(String message) {
    super(message);
  }
}
