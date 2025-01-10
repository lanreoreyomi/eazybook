package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.CheckoutItems;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CheckoutItemsRepository extends JpaRepository<CheckoutItems, Long> {

  CheckoutItems save(CheckoutItems checkoutItems);
  Optional<List<CheckoutItems>> findCheckoutItemsByBookIsbn(Long bookIsbn);
  Optional<List<CheckoutItems>> findCheckoutItemsByUsername(String username);
  void deleteCheckoutItemsByBookIsbn(Long bookIsbn);

}
