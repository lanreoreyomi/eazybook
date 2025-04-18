package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.controller.CheckoutItemsController;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.interfaces.ICheckoutStats;
import com.eazybooks.bookcatalogue.repository.CheckoutStatsRepository;
import jakarta.transaction.Transactional;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class checkoutStats implements ICheckoutStats {
  Logger logger = LoggerFactory.getLogger(checkoutStats.class);

  final CheckoutStatsRepository checkoutStatsRepository;
  final VerificationService verificationService;

  public checkoutStats(CheckoutStatsRepository checkoutStatsRepository,
      VerificationService verificationService) {
    this.checkoutStatsRepository = checkoutStatsRepository;
    this.verificationService = verificationService;
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
  public CheckoutStats getAllCheckoutStats(VerifyToken verifyTokenRequest)
      throws AuthorizationHeaderNotFound {

    if(Objects.isNull(verifyTokenRequest)) {
      logger.error("VerifyTokenRequest is null");
      throw new InvalidUserRequestException("VerifyTokenRequest is null");

    }
    verificationService.verifyUserToken(verifyTokenRequest);

    return checkoutStatsRepository.findCheckoutStatsWithMaxTotalCheckout().get(0);
  }

}