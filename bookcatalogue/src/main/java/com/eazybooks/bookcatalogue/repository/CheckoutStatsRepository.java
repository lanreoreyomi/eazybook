package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface CheckoutStatsRepository extends JpaRepository<CheckoutStats, Long> {

 CheckoutStats save(CheckoutStats checkoutStats);

 @Query("SELECT c FROM CheckoutStats c WHERE c.bookIsbn = :bookIsbn")
 CheckoutStats findByBookIsbn(Long bookIsbn);
}
