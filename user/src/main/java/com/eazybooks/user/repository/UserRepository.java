package com.eazybooks.user.repository;

import com.eazybooks.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import reactor.util.annotation.NonNullApi;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

   User findUserByUserId(Long userId);

   User findUserByUsername(String username);

   User findUserByEmail(String email);

   User save(User user);

}
