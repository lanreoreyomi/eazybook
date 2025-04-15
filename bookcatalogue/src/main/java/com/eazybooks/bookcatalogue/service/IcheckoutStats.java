package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutStats;
import com.eazybooks.bookcatalogue.repository.CheckoutStatsRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class IcheckoutStats implements ICheckoutStats {

   CheckoutStatsRepository checkoutStatsRepository;

  public IcheckoutStats(CheckoutStatsRepository checkoutStatsRepository) {
    this.checkoutStatsRepository = checkoutStatsRepository;
  }

  @Override
  public CheckoutStats save(CheckoutStats checkoutStats) {
    return checkoutStatsRepository.save(checkoutStats);
  }

  @Override
  public CheckoutStats findByIsbn(Long bookIsbn) {
    return checkoutStatsRepository.findByBookIsbn(bookIsbn);
  }

  @Override
  public int updateStats(CheckoutStats checkoutStats) {
    return checkoutStatsRepository.updateStats( checkoutStats.getTotalCheckout(), checkoutStats.getBookIsbn());
  }
  @Override
  public CheckoutStats getAllCheckoutStats() {
    return checkoutStatsRepository.findCheckoutStatsWithMaxTotalCheckout().get(0);
  }

}