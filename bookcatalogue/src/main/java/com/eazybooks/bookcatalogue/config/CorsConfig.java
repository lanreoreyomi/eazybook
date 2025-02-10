package com.eazybooks.bookcatalogue.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@EnableWebMvc
public class CorsConfig implements WebMvcConfigurer {

  @Override
  public void addCorsMappings(CorsRegistry registry) {
    registry.addMapping("/**")  // Allow all endpoints
        .allowedOrigins(
            "http://ec2-3-87-219-132.compute-1.amazonaws.com",
            "http://ec2-34-227-14-199.compute-1.amazonaws.com",
            "http://ec2-3-83-107-213.compute-1.amazonaws.com",
            "http://ec2-3-92-184-236.compute-1.amazonaws.com",
            "http://localhost:5173"
        ) // Frontend URL(s)
        .allowedMethods("*")
        .allowedHeaders("*")
        .allowCredentials(true)
        .exposedHeaders("Authorization");
  }
};
