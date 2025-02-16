package com.eazybooks.authentication.config;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.URL;
import java.net.URLConnection;
import java.net.UnknownHostException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
public class IpAddressConfig {

  @Bean
  @Profile("local")
  public IpAddressResolver localIpAddressResolver() {
    return () -> {
      try {
        return InetAddress.getLocalHost().getHostAddress();
      } catch (UnknownHostException e) {
        // Handle the exception appropriately
        return null;
      }
    };
  }

  @Bean
  @Profile("aws")
  public IpAddressResolver awsIpAddressResolver() {
    return () -> {
      try {
        URL url = new URL("http://169.254.169.254/latest/meta-data/local-ipv4");
        URLConnection connection = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
        String privateIp = in.readLine();
        in.close();
        return privateIp;
      } catch (Exception e) {
        // Handle exceptions appropriately
        return null;
      }
    };
  }
}

