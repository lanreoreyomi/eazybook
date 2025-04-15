package com.eazybooks.wishlist.service;

import com.eazybooks.wishlist.model.Wishlist;
import interfaces.IWishlist;
import com.eazybooks.wishlist.repository.WishlistRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class WishlistService implements IWishlist {

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
  public Wishlist findByBookIsbn(Long bookIsbn) {
    return wishlistRepository.findByIsbn(bookIsbn).orElse(null);
  }
  @Override
  public Wishlist findByBookIsbnAndUsername(Long bookIsbn, String username) {
    return wishlistRepository.findByIsbnAndUsername(bookIsbn, username).orElse(null);
  }


  @Override
  public void deleteById(Long id) {
  }

  @Override
  public void removeByBookIsbn(Long bookIsbn) {
    wishlistRepository.deleteByIsbn(bookIsbn);
  }

  @Override
  public List<Wishlist> findByUserName(String username) {
   return wishlistRepository.findByUsername(username).orElse(null);
  }


}
