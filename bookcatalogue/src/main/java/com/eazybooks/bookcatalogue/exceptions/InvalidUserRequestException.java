package com.eazybooks.bookcatalogue.exceptions;

public class InvalidUserRequestException extends RuntimeException {

  public InvalidUserRequestException(String message) {
    super(message);
  }
}
