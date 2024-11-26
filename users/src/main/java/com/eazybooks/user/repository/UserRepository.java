package com.eazybooks.user.repository;

import com.eazybooks.user.model.Users;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends JpaRepository<Users, Long> {



   Users getUsersByUserId(Long userId);

   Users getUsersByUsername(String username);

   Users getUsersByEmail(String email);


}
