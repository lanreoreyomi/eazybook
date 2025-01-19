package com.eazybooks.authentication.config;

@FunctionalInterface
public interface IpAddressResolver {
  String getIpAddress();
}
