package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.Checkout;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
@SuppressWarnings("ALL")
public interface CheckoutRepository extends JpaRepository<Checkout, Long> {

 Checkout save(Checkout checkout);
 
 List<Checkout> findCheckoutsByCheckedOutBy(String username);
 Checkout findCheckoutById(Long id);


}