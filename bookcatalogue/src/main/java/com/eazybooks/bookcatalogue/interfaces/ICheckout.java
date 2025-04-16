package com.eazybooks.bookcatalogue.interfaces;


import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.model.CheckoutInfo;
import java.util.List;

public interface ICheckout {

  Checkout save(Checkout checkout);

  List<Checkout> findCheckoutsByCheckedOutBy(String username);

  Checkout updateCheckout(Checkout checkout);

  String handleBookReturns(String username, Long bookIsbn, VerifyToken verifyTokenRequest)
      throws BookNotFoundException, AuthorizationHeaderNotFound;

  List<CheckoutInfo> getCheckoutInfo(VerifyToken verifyTokenRequest, String username)
      throws AuthorizationHeaderNotFound;
}
