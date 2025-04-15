package com.eazybooks.bookcatalogue.interfaces;


import com.eazybooks.bookcatalogue.model.Checkout;
import java.util.List;

public interface ICheckout {

  Checkout save(Checkout checkout);

  List<Checkout> findCheckoutsByCheckedOutBy(String username);

  Checkout updateCheckout(Checkout checkout);
}
