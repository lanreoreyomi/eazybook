package com.eazybooks.bookcatalogue.exceptions;

public class UserNotAdminException extends RuntimeException {
  public UserNotAdminException(String message) {
    super(message);
  }

}
