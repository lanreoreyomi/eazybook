package com.eazybooks.bookcatalogue.exceptions;

public class EmptyBookRequestException extends RuntimeException {
  public EmptyBookRequestException(String message) {
    super(message);
  }

}
