package com.eazybooks.authentication.controller;

import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
public class UserControllerTest {

//  @Test
//  public void testCreateUser() {
//    AuthenticatorRepository authenticatorRepository = Mockito.mock(AuthenticatorRepository.class);
//    HashService hashService = Mockito.mock(HashService.class);
//    AuthenticatorService userService = new AuthenticatorService(authenticatorRepository, hashService);
//
//    User user = new User();
//    user.setEmail("testuser@gmail.com");
//    user.setUsername("testuser");
//    user.setPassword("testpassword");
//    user.setLastname("testlastname");
//    user.setFirstname("testfirstname");
//
//    when(authenticatorRepository.save(user)).thenReturn(user);
//
//    final User createdUser = userService.createUser(user);
//    Assert.notNull(createdUser, "User should not be null");
//    assertEquals(user.getUsername(), createdUser.getUsername());
//    verify(authenticatorRepository).save(user);
//  }

//  @Test
//  @Ignore
//  public void testFindUserById() {
//    AuthenticatorRepository authenticatorRepository = Mockito.mock(AuthenticatorRepository.class);
//    HashService hashService = Mockito.mock(HashService.class);
//    AuthenticatorService userService = new AuthenticatorService(authenticatorRepository, hashService);
//
//    User user = new User();
//    user.setUserId(1L);
//    user.setEmail("testuser@gmail.com");
//    user.setUsername("testuser");
//    user.setPassword("testpassword");
//    user.setLastname("testlastname");
//    user.setFirstname("testfirstname");
//
//    when(userService.findUserByUserId(1L)).thenReturn(user);
//
//    final User createdUser = userService.findUserByUserId(user.getUserId());
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
//    User user = new User();
//    user.setUserId(1L);
//    user.setEmail("testuser@gmail.com");
//    user.setUsername("testuser");
//    user.setPassword("testpassword");
//    user.setLastname("testlastname");
//    user.setFirstname("testfirstname");
//
//    when(userService.findByUsername(user.getUsername())).thenReturn(user);
//
//    final User createdUser = userService.findByUsername(user.getUsername());
//    Assert.notNull(createdUser, "User should not be null");
//    assertEquals(user.getUsername(), createdUser.getUsername());
//    verify(userRepository).findUsersByUsername(user.getUsername());
//  }
//

