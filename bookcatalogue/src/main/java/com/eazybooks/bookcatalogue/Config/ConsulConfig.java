package com.eazybooks.bookcatalogue.Config;

import com.ecwid.consul.v1.agent.model.NewService;
import org.springframework.cloud.consul.serviceregistry.ConsulRegistrationCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ConsulConfig {

  @Bean
  public ConsulRegistrationCustomizer customizer() {
    return registration -> {
      NewService service = registration.getService();
      service.setAddress("bookcatalogue"); // Set the desired hostname
    };
  }
}