package com.eazybooks.bookcatalogue.interfaces;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.BookNotFoundException;
import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.CheckoutItems;
 import java.util.List;

public interface ICheckoutItems {
  CheckoutItems save(CheckoutItems checkoutItems);
  List<CheckoutItems>  findCheckoutItemsByBookIsbn(Long bookId);
  List<CheckoutItems> findCheckoutItemsByUsername(String username);
  void deleteCheckoutItemsByBookIsbn(Long bookIsbn);
  String addBookItemsToCheckout(VerifyToken verifyTokenRequest, Long bookisbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException;
  List<BookCatalogue> checkoutItemsForUser(VerifyToken verifyTokenRequest)
      throws AuthorizationHeaderNotFound;
  String removeCheckoutItems(VerifyToken verifyTokenRequest,
      Long bookisbn) throws AuthorizationHeaderNotFound, BookNotFoundException;
}
