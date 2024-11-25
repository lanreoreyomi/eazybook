package com.eazybooks.user.repository;

import com.eazybooks.user.model.User;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.FluentQuery.FetchableFluentQuery;

public interface UserRepository extends JpaRepository<User, Long> {


   User createUser(User user);

   User findUserByUserId(String userId);

   User findByUsername(String username);
   User findByEmail(String email);


}
