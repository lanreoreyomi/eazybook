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
  Optional<Wishlist> findByUserId(Long userId);
  List<Wishlist> findByBookCatalogueId(Long bookCatalogueId);

  @Query("SELECT i FROM Wishlist i WHERE i.bookIsbn = :bookIsbn")
  Optional<Wishlist> findByBookIsbn(Long bookIsbn);

  void deleteById(Long id);
  void deleteByUserId(Long userId);
  void deleteByBookCatalogueId(Long bookCatalogueId);
  void deleteByBookIsbn(Long bookIsbn);

}
