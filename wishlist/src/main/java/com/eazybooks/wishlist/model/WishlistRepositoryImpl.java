package com.eazybooks.wishlist.model;

import java.util.List;
import java.util.Optional;

public interface WishlistRepositoryImpl {

  Wishlist save(Wishlist wishlist);
  Wishlist findById(Long id);
  List<Wishlist> findByUserName(String username);
  Wishlist findByBookIsbnAndUsername(Long isbn, String username);
  Wishlist findByBookIsbn(Long bookIsbn);
  void deleteById(Long id);
  void removeByBookIsbn(Long bookIsbn);
}
