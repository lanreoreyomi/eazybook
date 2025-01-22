package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CheckoutStatsRepository extends JpaRepository<CheckoutStats, Long> {

 CheckoutStats save(CheckoutStats checkoutStats);

 @Query("SELECT c FROM CheckoutStats c WHERE c.bookIsbn = :bookIsbn")
 CheckoutStats findByBookIsbn(Long bookIsbn);

 @Modifying
 @Query("UPDATE CheckoutStats c SET c.totalCheckout = :totalCheckout WHERE c.bookIsbn = :bookIsbn")
 int updateStats(@Param("totalCheckout") int totalCheckout, @Param("bookIsbn") Long bookIsbn);

 @Query("SELECT cs FROM CheckoutStats cs WHERE cs.totalCheckout = (SELECT max(totalCheckout) FROM CheckoutStats )")
 List<CheckoutStats> findCheckoutStatsWithMaxTotalCheckout();

}