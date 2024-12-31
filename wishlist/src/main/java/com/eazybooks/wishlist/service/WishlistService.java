package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.model.Wishlist;
import com.eazybooks.wishlist.model.WishlistRepositoryImpl;
import com.eazybooks.wishlist.repository.WishlistRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class WishlistService  implements WishlistRepositoryImpl {

  WishlistRepository wishlistRepository;

  public WishlistService(WishlistRepository wishlistRepository) {
    this.wishlistRepository = wishlistRepository;
  }

  @Override
  public Wishlist save(Wishlist wishlist) {
    return wishlistRepository.save(wishlist);
  }

  @Override
  public Wishlist findById(Long id) {
    return null;
  }

  @Override
  public Wishlist findByUserId(Long userId) {
    return null;
  }

  @Override
  public List<Wishlist> findByBookCatalogueId(Long catalogueId) {
    return wishlistRepository.findByBookCatalogueId(catalogueId);
  }

  @Override
  public Wishlist findByBookIsbn(Long bookIsbn) {
    return wishlistRepository.findByBookIsbn(bookIsbn).orElse(null);
  }

  @Override
  public void deleteById(Long id) {

  }

  @Override
  public void deleteByUserId(Long userId) {

  }

  @Override
  public void deleteByBookIsbn(Long bookIsbn) {

  }
}
