package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.model.CheckoutStatsImpl;
import com.eazybooks.bookcatalogue.model.MaxCheckoutResponse;
import com.eazybooks.bookcatalogue.repository.CheckoutStatsRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class CheckoutStatsService implements CheckoutStatsImpl {

  private static final Logger log = LoggerFactory.getLogger(CheckoutStatsService.class);
  CheckoutStatsRepository checkoutStatsRepository;

  public CheckoutStatsService(CheckoutStatsRepository checkoutStatsRepository) {
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

    final List<CheckoutStats> checkoutStatsWithMaxTotalCheckout =
        checkoutStatsRepository.findCheckoutStatsWithMaxTotalCheckout();
    return !checkoutStatsWithMaxTotalCheckout.isEmpty() ? checkoutStatsWithMaxTotalCheckout.get(0) : null;
  }


}
