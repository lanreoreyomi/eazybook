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

  @Query("SELECT i FROM Wishlist i WHERE i.bookIsbn = :bookIsbn")
  Optional<Wishlist> findByBookIsbn(Long bookIsbn);

  @Query("SELECT i FROM Wishlist i WHERE i.bookIsbn = :bookIsbn and i.username = :username")
  Optional<Wishlist> findByBookIsbnAndUsername(Long bookIsbn, String username);

  void deleteById(Long id);
  void deleteByBookIsbn(Long bookIsbn);

}
