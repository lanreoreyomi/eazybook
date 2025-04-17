package com.eazybooks.wishlist.exceptions;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class GlobalExceptionHandler {
  Logger logger = LoggerFactory.getLogger(GlobalExceptionHandler.class);

  @ExceptionHandler(InvalidUserRequestException.class)
  public ResponseEntity<String> handleInvalidUserRequestException(InvalidUserRequestException ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.BAD_REQUEST);
  }
  @ExceptionHandler(InternalServerException.class)
  public ResponseEntity<String> handleInternalServerException(InternalServerException ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
  }

  @ExceptionHandler(InvalidUserTokenException.class)
  public ResponseEntity<String> handleInvalidUserTokenException(InvalidUserTokenException ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.UNAUTHORIZED);
  }

  @ExceptionHandler(BookNotFoundException.class)
  public ResponseEntity<String> handleBookNotFoundException(BookNotFoundException ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.NOT_FOUND);
  }

  @ExceptionHandler(AuthorizationHeaderNotFound.class)
  public ResponseEntity<String> handleAuthorizationHeaderNotFound(
      AuthorizationHeaderNotFound ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.BAD_REQUEST);
  }
  @ExceptionHandler(BookExistException.class)
  public ResponseEntity<String> handleBookExistException(
      BookExistException ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.CONFLICT);
  }
  @ExceptionHandler(UserNotFoundException.class)
  public ResponseEntity<String> handleUserNotFoundException(
      UserNotFoundException ex) {
    logger.error(ex.getMessage());
    return new ResponseEntity<>(ex.getMessage(), HttpStatus.NOT_FOUND);
  }


}
