package com.eazybooks.user.config;

import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

@Configuration
  public class ServicesConfig {

    @Bean
    @LoadBalanced
    public RestTemplate loadBalancedRestTemplate() {
      return new RestTemplate();
    }

  @Bean
  public RestTemplate standardRestTemplate() {
    return new RestTemplate();
  }
  }


