package com.eazybooks.bookcatalogue.exceptions;

public class BookExistInCheckoutException extends RuntimeException {

  public BookExistInCheckoutException(String message) {
    super(message);
  }
}
