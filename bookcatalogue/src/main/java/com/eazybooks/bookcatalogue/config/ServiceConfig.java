package com.eazybooks.bookcatalogue.config;

import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;
@Configuration
public class ServiceConfig {

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

