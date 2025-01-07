package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.model.CheckoutStatsImpl;
import com.eazybooks.bookcatalogue.repository.CheckoutStatsRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class CheckoutStatsService implements CheckoutStatsImpl {

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
}
