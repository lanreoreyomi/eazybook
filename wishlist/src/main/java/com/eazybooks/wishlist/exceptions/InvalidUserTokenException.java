package com.eazybooks.wishlist.exceptions;

public class InvalidUserTokenException extends RuntimeException {

  public InvalidUserTokenException(String message) {
    super(message);
  }
}
