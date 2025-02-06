package com.eazybooks.user.config;

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
            "http://ec2-52-91-196-70.compute-1.amazonaws.com",
            "https://eazybook.s3.us-east-1.amazonaws.com",
            "http://localhost:5173"
        ) // Frontend URL(s)
        .allowedMethods("*")
        .allowedHeaders("*")
        .allowCredentials(true)
        .exposedHeaders("Authorization");
  }
};
