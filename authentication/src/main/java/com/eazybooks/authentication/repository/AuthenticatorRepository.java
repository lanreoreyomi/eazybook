package com.eazybooks.authentication.repository;


import com.eazybooks.authentication.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
 import org.springframework.stereotype.Repository;

@Repository
public interface AuthenticatorRepository extends JpaRepository<User, String> {

  User findUserByUsername(String username);

  User findUserByEmail(String email);

  
  User save(User user);

  void deleteUserByUsername(String username);
}
