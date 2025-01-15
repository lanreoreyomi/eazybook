package com.eazybooks.authentication.config.RestUtils;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class RestUtils {

  public static String get_USER_createUserUrl() throws UnknownHostException {
    return "http://"+InetAddress.getLocalHost().getHostAddress()+":9087/user/create-account";
  }

}
