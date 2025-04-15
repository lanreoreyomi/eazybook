package com.eazybooks.bookcatalogue.interfaces;

import com.eazybooks.bookcatalogue.model.CheckoutItems;
import java.util.List;

public interface ICheckoutItems {
  CheckoutItems save(CheckoutItems checkoutItems);
  List<CheckoutItems>  findCheckoutItemsByBookIsbn(Long bookId);
  List<CheckoutItems> findCheckoutItemsByUsername(String username);
  void deleteCheckoutItemsByBookIsbn(Long bookIsbn);
}
