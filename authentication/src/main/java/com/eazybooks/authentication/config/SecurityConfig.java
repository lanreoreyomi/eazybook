package com.eazybooks.authentication.config;

//import static org.springframework.security.config.Customizer.withDefaults;
//
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
//import org.springframework.security.config.annotation.web.builders.HttpSecurity;
//import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
//import org.springframework.security.core.userdetails.User;
//import org.springframework.security.core.userdetails.UserDetails;
//import org.springframework.security.core.userdetails.UserDetailsService;
//import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
//import org.springframework.security.crypto.password.PasswordEncoder;
//import org.springframework.security.provisioning.InMemoryUserDetailsManager;
//import org.springframework.security.web.SecurityFilterChain;

import com.eazybooks.authentication.Filter.JwtAuthenticationFilter;
import com.eazybooks.authentication.UserDetails.UserDetailsService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
//@EnableMethodSecurity
public class SecurityConfig {

  private final UserDetailsService userDetailsService;
  private final JwtAuthenticationFilter jwtAuthenticationFilter;

  public SecurityConfig(UserDetailsService userDetailsService,
      JwtAuthenticationFilter jwtAuthenticationFilter) {
    this.userDetailsService = userDetailsService;
    this.jwtAuthenticationFilter = jwtAuthenticationFilter;
  }

  @Bean
  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
    return http
        .csrf(AbstractHttpConfigurer::disable)
        .authorizeHttpRequests(
            req->
                req.requestMatchers("/auth/login/**",
                        "/auth/create-account/**",
                        "/welcome**",
                        "/actuator/**")
                    .permitAll()
                    .requestMatchers("/admin_only_pages/**").hasAuthority("ADMIN")
                    .anyRequest()
                    .authenticated()
        ).userDetailsService(userDetailsService)
        .sessionManagement(session ->
            session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
        .addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class)
         .build();

  }

//  @Bean
//  public UserDetailsService userDetailsService(PasswordEncoder encoder) {
//    // InMemoryUserDetailsManager setup with two users
//    UserDetails admin = User.withUsername("Amiya")
//        .password(encoder.encode("123"))
//        .roles("ADMIN", "USER")
//        .build();
//
//    UserDetails user = User.withUsername("Ejaz")
//        .password(encoder.encode("123"))
//        .roles("USER")
//        .build();
//
//    return new InMemoryUserDetailsManager(admin, user);
//  }
//  // Configuring HttpSecurity
//  @Bean
//  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
//    http
//        .csrf(csrf -> csrf.disable()) // Disable CSRF for simplicity
//        .authorizeHttpRequests(auth -> auth
//            .requestMatchers("/auth/welcome").permitAll() // Permit all access to /auth/welcome
//            .requestMatchers("/auth/user/**").authenticated() // Require authentication for /auth/user/**
//            .requestMatchers("/auth/admin/**").authenticated() // Require authentication for /auth/admin/**
//        )
//        .formLogin(withDefaults()); // Enable form-based login
//
//    return http.build();
//  }
  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder();
  }

  @Bean
  public AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration)
      throws Exception {
    return authenticationConfiguration.getAuthenticationManager();
  }

}

