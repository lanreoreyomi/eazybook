package com.eazybooks.bookcatalogue.exceptions;

public class BookNotEligibleForRentException extends RuntimeException {

  public BookNotEligibleForRentException(String message) {
    super(message);
  }
}
