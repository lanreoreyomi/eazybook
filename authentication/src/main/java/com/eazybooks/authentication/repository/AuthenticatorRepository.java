package com.eazybooks.authentication.repository;

import com.eazybooks.authentication.model.Users;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface AuthenticatorRepository extends JpaRepository<Users, Long> {

  Users findUsersByUsername(String username);

  Users findUsersByEmail(String email);

  Users save(Users user);

}
