package com.eazybooks.wishlist.exceptions;

public class InvalidUserRequestException extends RuntimeException {

  public InvalidUserRequestException(String message) {
    super(message);
  }
}
