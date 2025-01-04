package com.eazybooks.wishlist.repository;

import com.eazybooks.wishlist.model.Wishlist;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface WishlistRepository extends JpaRepository<Wishlist, Long> {

  Optional<Wishlist> findById(Long id);

  @Query("SELECT i FROM Wishlist i WHERE i.username = :username")
  Optional<List<Wishlist>> findByUsername(String username);

  @Query("SELECT i FROM Wishlist i WHERE i.isbn = :isbn")
  Optional<Wishlist> findByIsbn(Long isbn);

  @Query("SELECT i FROM Wishlist i WHERE i.isbn = :isbn and i.username = :username")
  Optional<Wishlist> findByIsbnAndUsername(Long isbn, String username);

  void deleteById(Long id);
  void deleteByIsbn(Long isbn);

}
