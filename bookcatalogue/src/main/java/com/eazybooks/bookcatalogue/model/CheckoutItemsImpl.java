package com.eazybooks.bookcatalogue.model;

import java.util.List;
import java.util.Optional;

public interface CheckoutItemsImpl {
  CheckoutItems save(CheckoutItems checkoutItems);
  List<CheckoutItems>  findCheckoutItemsByBookIsbn(Long bookId);
  List<CheckoutItems> findCheckoutItemsByUsername(String username);
  void deleteCheckoutItemsByBookIsbn(Long bookIsbn);
}
