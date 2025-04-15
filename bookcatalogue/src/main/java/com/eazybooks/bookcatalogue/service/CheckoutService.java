package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.Checkout;
import com.eazybooks.bookcatalogue.interfaces.ICheckout;
import com.eazybooks.bookcatalogue.repository.CheckoutRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class CheckoutService implements ICheckout {

  private final CheckoutRepository checkoutRepository;

  public CheckoutService(CheckoutRepository checkoutRepository) {
    this.checkoutRepository = checkoutRepository;
  }

  @Override
  public Checkout save(Checkout checkout) {
    return checkoutRepository.save(checkout);
  }
  @Override
  public Checkout updateCheckout(Checkout checkout) {
    return checkoutRepository.save(checkout);
  }

  @Override
  public List<Checkout> findCheckoutsByCheckedOutBy(String username) {
    return checkoutRepository.findCheckoutsByCheckedOutBy(username);
  }

}
