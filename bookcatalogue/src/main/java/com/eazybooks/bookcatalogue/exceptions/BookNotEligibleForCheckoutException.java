package com.eazybooks.bookcatalogue.exceptions;

public class BookNotEligibleForCheckoutException extends RuntimeException {

  public BookNotEligibleForCheckoutException(String message) {
    super(message);
  }
}
