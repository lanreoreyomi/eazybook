package com.eazybooks.bookcatalogue.model;


public interface CheckoutStatsImpl {

  CheckoutStats save(CheckoutStats checkoutStats);
  CheckoutStats findByIsbn(Long isbn);

}