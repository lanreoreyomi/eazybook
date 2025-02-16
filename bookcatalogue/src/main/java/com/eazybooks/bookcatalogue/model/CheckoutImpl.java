package com.eazybooks.bookcatalogue.model;


import java.util.List;

public interface CheckoutImpl {

  Checkout save(Checkout checkout);

  List<Checkout> findCheckoutsByCheckedOutBy(String username);

  Checkout updateCheckout(Checkout checkout);
}
