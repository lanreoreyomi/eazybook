package com.eazybooks.user.Utils;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class RestUtils {
  public static String get_AUTH_ValidateTokenUrl() throws UnknownHostException {
    return "http://"+ InetAddress.getLocalHost().getHostAddress()+":9084/auth/validate-token";
  }
}
