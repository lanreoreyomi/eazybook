package com.eazybooks.wishlist.utils;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class RestUtils {

  public static String get_AUTH_ValidateTokenUrl() throws UnknownHostException {
    return "http://"+ InetAddress.getLocalHost().getHostAddress()+":9084/auth/validate-token";
  }
    public static String get_BOOK_IsbnUrl(Long bookIsbn) throws UnknownHostException {
     return "http://"+ InetAddress.getLocalHost().getHostAddress()+":9189/bookcatalogue/isbn/"+ bookIsbn;
  }

}
