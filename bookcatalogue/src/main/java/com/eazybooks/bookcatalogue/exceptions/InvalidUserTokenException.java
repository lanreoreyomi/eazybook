package com.eazybooks.bookcatalogue.exceptions;

public class InvalidUserTokenException extends RuntimeException {

  public InvalidUserTokenException(String message) {
    super(message);
  }
}
