package com.eazybooks.bookcatalogue.model;


import java.util.List;

public interface CheckoutStatsImpl {

  CheckoutStats save(CheckoutStats checkoutStats);
  CheckoutStats findByIsbn(Long isbn);
  int updateStats(CheckoutStats checkoutStats);
  CheckoutStats getAllCheckoutStats();
}
