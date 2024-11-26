package com.eazybooks.user.repository;

import com.eazybooks.user.model.Users;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends JpaRepository<Users, Long> {

   Users findUsersByUserId(Long userId);

   Users findUsersByUsername(String username);

   Users findUsersByEmail(String email);

   Users save(Users user);

}
