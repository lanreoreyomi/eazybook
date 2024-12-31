package com.eazybooks.wishlist.model;

import java.util.List;

public interface WishlistRepositoryImpl {

  Wishlist save(Wishlist wishlist);
  Wishlist findById(Long id);
  Wishlist findByUserId(Long userId);
  List<Wishlist> findByBookCatalogueId(Long bookCatalogueId);
  Wishlist findByBookIsbn(Long bookIsbn);
  void deleteById(Long id);
  void deleteByUserId(Long userId);
  void deleteByBookIsbn(Long bookIsbn);
}
