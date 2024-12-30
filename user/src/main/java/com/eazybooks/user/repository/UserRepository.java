package com.eazybooks.user.repository;

import com.eazybooks.user.model.User;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

   Optional<User> findById(Long userId);

   Optional<User> findByUsername(String username);

   Optional<User> findByEmail(String email);

   User save(User user);

}
