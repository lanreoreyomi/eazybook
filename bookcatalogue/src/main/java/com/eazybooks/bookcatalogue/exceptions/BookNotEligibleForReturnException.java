package com.eazybooks.bookcatalogue.exceptions;

public class BookNotEligibleForReturnException extends RuntimeException {

  public BookNotEligibleForReturnException(String message) {
    super(message);
  }
}
