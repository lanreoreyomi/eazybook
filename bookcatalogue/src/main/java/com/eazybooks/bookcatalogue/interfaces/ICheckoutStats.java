package com.eazybooks.bookcatalogue.interfaces;


import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.model.CheckoutStats;

public interface ICheckoutStats {
  CheckoutStats save(CheckoutStats checkoutStats);
  CheckoutStats findByIsbn(Long isbn);
  int updateStats(CheckoutStats checkoutStats);
  CheckoutStats getAllCheckoutStats(VerifyToken verifyTokenRequest)
      throws AuthorizationHeaderNotFound;

}
