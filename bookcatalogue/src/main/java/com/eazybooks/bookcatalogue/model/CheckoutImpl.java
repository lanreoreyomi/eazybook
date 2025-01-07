package com.eazybooks.bookcatalogue.model;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.Query;


public interface CheckoutImpl {

  Checkout save(Checkout checkout);

  List<Checkout> findCheckoutsByCheckedOutBy(String username);

  Checkout updateCheckout(Checkout checkout);
}
