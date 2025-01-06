package com.eazybooks.bookcatalogue.repository;

import com.eazybooks.bookcatalogue.model.Checkout;
import java.util.List;
 import org.springframework.data.jpa.repository.JpaRepository;

public interface CheckoutRepository extends JpaRepository<Checkout, Long> {

}
