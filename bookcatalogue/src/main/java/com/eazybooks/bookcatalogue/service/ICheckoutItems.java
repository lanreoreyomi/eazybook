package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.CheckoutItems;
import com.eazybooks.bookcatalogue.repository.CheckoutItemsRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class ICheckoutItems implements com.eazybooks.bookcatalogue.interfaces.ICheckoutItems {

  CheckoutItemsRepository checkoutItemsRepository;

  public ICheckoutItems(CheckoutItemsRepository checkoutItemsRepository) {
    this.checkoutItemsRepository = checkoutItemsRepository;
  }

  @Override
  public CheckoutItems save(CheckoutItems checkoutItems) {
    return checkoutItemsRepository.save(checkoutItems);
  }

  @Override
  public List<CheckoutItems> findCheckoutItemsByBookIsbn(Long bookId) {
    return checkoutItemsRepository.findCheckoutItemsByBookIsbn(bookId).get();
  }

  @Override
  public List<CheckoutItems> findCheckoutItemsByUsername(String username) {
    return checkoutItemsRepository.findCheckoutItemsByUsername(username).get();
  }

  @Override
  public void deleteCheckoutItemsByBookIsbn(Long bookIsbn) {
  checkoutItemsRepository.deleteCheckoutItemsByBookIsbn(bookIsbn);
  }
}
