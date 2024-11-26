package com.eazybooks.user.controller;

import static org.mockito.Mockito.when;

import com.eazybooks.user.model.Users;
import com.eazybooks.user.repository.UserRepository;
import com.eazybooks.user.service.HashService;
import com.eazybooks.user.service.UserService;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
public class UserControllerTest {

  @Test
  public void testCreateUser() {
    UserRepository userRepository = Mockito.mock(UserRepository.class);
    HashService hashService = Mockito.mock(HashService.class);
    UserService userService = new UserService(userRepository, hashService);

    Users user = new Users();
    user.setEmail("testuser@gmail.com");
    user.setUsername("testuser");
    user.setPassword("testpassword");
    user.setLastname("testlastname");
    user.setFirstname("testfirstname");

    when(userRepository.save(user)).thenReturn(user);

    userService.createUser(user);
//
//    assertEquals("testuser", createdUser.getUsername());
//    verify(userRepository).save(user);
  }

}
