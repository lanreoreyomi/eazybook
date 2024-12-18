package com.eazybooks.authentication.controller;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.authentication.model.Users;
import com.eazybooks.authentication.repository.AuthenticatorRepository;
import com.eazybooks.authentication.service.AuthenticatorService;
import com.eazybooks.authentication.service.HashService;
import org.hibernate.annotations.IdGeneratorType;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.util.Assert;

@SpringBootTest
public class UserControllerTest {

  @Test
  public void testCreateUser() {
    AuthenticatorRepository authenticatorRepository = Mockito.mock(AuthenticatorRepository.class);
    HashService hashService = Mockito.mock(HashService.class);
    AuthenticatorService userService = new AuthenticatorService(authenticatorRepository, hashService);

    Users user = new Users();
    user.setEmail("testuser@gmail.com");
    user.setUsername("testuser");
    user.setPassword("testpassword");
    user.setLastname("testlastname");
    user.setFirstname("testfirstname");

    when(authenticatorRepository.save(user)).thenReturn(user);

    final Users createdUser = userService.createUser(user);
    Assert.notNull(createdUser, "User should not be null");
    assertEquals(user.getUsername(), createdUser.getUsername());
    verify(authenticatorRepository).save(user);
  }

//  @Test
//  @Ignore
//  public void testFindUserById() {
//    AuthenticatorRepository authenticatorRepository = Mockito.mock(AuthenticatorRepository.class);
//    HashService hashService = Mockito.mock(HashService.class);
//    AuthenticatorService userService = new AuthenticatorService(authenticatorRepository, hashService);
//
//    Users user = new Users();
//    user.setUserId(1L);
//    user.setEmail("testuser@gmail.com");
//    user.setUsername("testuser");
//    user.setPassword("testpassword");
//    user.setLastname("testlastname");
//    user.setFirstname("testfirstname");
//
//    when(userService.findUserByUserId(1L)).thenReturn(user);
//
//    final Users createdUser = userService.findUserByUserId(user.getUserId());
//    Assert.notNull(createdUser, "User should not be null");
//    assertEquals(1L, user.getUserId());
//    verify(userRepository).findUsersByUserId(1L);
  }
//  @Test
//  public void testFindUserByUsername() {
//    AuthenticatorRepository authenticatorRepository = Mockito.mock(UserRepository.class);
//    HashService hashService = Mockito.mock(HashService.class);
//    AuthenticatorService userService = new AuthenticatorService(    AuthenticatorRepository authenticatorRepository = Mockito.mock(UserRepository.class);
//, hashService);
//
//    Users user = new Users();
//    user.setUserId(1L);
//    user.setEmail("testuser@gmail.com");
//    user.setUsername("testuser");
//    user.setPassword("testpassword");
//    user.setLastname("testlastname");
//    user.setFirstname("testfirstname");
//
//    when(userService.findByUsername(user.getUsername())).thenReturn(user);
//
//    final Users createdUser = userService.findByUsername(user.getUsername());
//    Assert.notNull(createdUser, "User should not be null");
//    assertEquals(user.getUsername(), createdUser.getUsername());
//    verify(userRepository).findUsersByUsername(user.getUsername());
//  }
//

