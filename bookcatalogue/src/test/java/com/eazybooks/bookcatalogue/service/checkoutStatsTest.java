package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.DTO.VerifyToken;
import com.eazybooks.bookcatalogue.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserRequestException;
import com.eazybooks.bookcatalogue.exceptions.InvalidUserTokenException;
import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.repository.CheckoutStatsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
 import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class checkoutStatsTest {

  @Mock
  private CheckoutStatsRepository checkoutStatsRepository;

  @Mock
  private VerificationService verificationService;

  @InjectMocks
  private checkoutStats checkoutStatsService;

  private CheckoutStats sampleCheckoutStats;
  private VerifyToken sampleVerifyToken;
  private final Long bookIsbn = 1234567890L;
  private final String validToken = "Bearer valid-jwt-token";
  private final String username = "testuser";

  @BeforeEach
  void setUp() {
    sampleVerifyToken = new VerifyToken(validToken, username);

    sampleCheckoutStats = new CheckoutStats();
    sampleCheckoutStats.setBookIsbn(bookIsbn);
    sampleCheckoutStats.setTitle("Test Book Title");
    sampleCheckoutStats.setTotalCheckout(5);
  }

  @Test
  @DisplayName("save should delegate to repository save")
  void save_DelegatesToRepository() {
    when(checkoutStatsRepository.save(any(CheckoutStats.class))).thenReturn(sampleCheckoutStats);

    CheckoutStats result = checkoutStatsService.save(sampleCheckoutStats);

    assertNotNull(result);
    assertEquals(sampleCheckoutStats, result);
    verify(checkoutStatsRepository, times(1)).save(sampleCheckoutStats);
  }

  @Test
  @DisplayName("findByIsbn should return stats when found")
  void findByIsbn_Found_ReturnsStats() {
    when(checkoutStatsRepository.findByBookIsbn(bookIsbn)).thenReturn(sampleCheckoutStats);

    CheckoutStats result = checkoutStatsService.findByIsbn(bookIsbn);

    assertNotNull(result);
    assertEquals(sampleCheckoutStats, result);
    verify(checkoutStatsRepository, times(1)).findByBookIsbn(bookIsbn);
  }

  @Test
  @DisplayName("findByIsbn should return null when not found")
  void findByIsbn_NotFound_ReturnsNull() {
    when(checkoutStatsRepository.findByBookIsbn(bookIsbn)).thenReturn(null);

    CheckoutStats result = checkoutStatsService.findByIsbn(bookIsbn);

    assertNull(result);
    verify(checkoutStatsRepository, times(1)).findByBookIsbn(bookIsbn);
  }

  @Test
  @DisplayName("updateStats should delegate to repository updateStats")
  void updateStats_DelegatesToRepository() {
    int expectedUpdateCount = 1;
    when(checkoutStatsRepository.updateStats(sampleCheckoutStats.getTotalCheckout(), sampleCheckoutStats.getBookIsbn()))
        .thenReturn(expectedUpdateCount);

    int result = checkoutStatsService.updateStats(sampleCheckoutStats);

    assertEquals(expectedUpdateCount, result);
    verify(checkoutStatsRepository, times(1)).updateStats(sampleCheckoutStats.getTotalCheckout(), sampleCheckoutStats.getBookIsbn());
  }

  @Test
  @DisplayName("getAllCheckoutStats should return top stats when token is valid")
  void getAllCheckoutStats_ValidToken_ReturnsStats() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(checkoutStatsRepository.findCheckoutStatsWithMaxTotalCheckout()).thenReturn(List.of(sampleCheckoutStats));

    CheckoutStats result = checkoutStatsService.getAllCheckoutStats(sampleVerifyToken);

    assertNotNull(result);
    assertEquals(sampleCheckoutStats, result);
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(checkoutStatsRepository, times(1)).findCheckoutStatsWithMaxTotalCheckout();
  }

  @Test
  @DisplayName("getAllCheckoutStats should throw InvalidUserRequestException for null token")
  void getAllCheckoutStats_NullToken_ThrowsInvalidUserRequestException() {
    InvalidUserRequestException exception = assertThrows(InvalidUserRequestException.class, () -> {
      checkoutStatsService.getAllCheckoutStats(null);
    });

    assertEquals("VerifyTokenRequest is null", exception.getMessage());
    verifyNoInteractions(verificationService, checkoutStatsRepository);
  }

  @Test
  @DisplayName("getAllCheckoutStats should throw exception if token verification fails")
  void getAllCheckoutStats_TokenVerificationFails_ThrowsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenThrow(new InvalidUserTokenException("Token invalid"));

    InvalidUserTokenException exception = assertThrows(InvalidUserTokenException.class, () -> {
      checkoutStatsService.getAllCheckoutStats(sampleVerifyToken);
    });

    assertEquals("Token invalid", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verifyNoInteractions(checkoutStatsRepository);
  }

  @Test
  @DisplayName("getAllCheckoutStats should throw IndexOutOfBoundsException if repository returns empty list")
  void getAllCheckoutStats_RepoReturnsEmptyList_ThrowsIndexOutOfBoundsException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(checkoutStatsRepository.findCheckoutStatsWithMaxTotalCheckout()).thenReturn(Collections.emptyList());

    assertThrows(IndexOutOfBoundsException.class, () -> {
      checkoutStatsService.getAllCheckoutStats(sampleVerifyToken);
    });

    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(checkoutStatsRepository, times(1)).findCheckoutStatsWithMaxTotalCheckout();
  }

  @Test
  @DisplayName("getAllCheckoutStats should propagate RuntimeException from repository")
  void getAllCheckoutStats_RepoThrowsRuntimeException_PropagatesException() throws AuthorizationHeaderNotFound {
    when(verificationService.verifyUserToken(any(VerifyToken.class))).thenReturn(Boolean.TRUE);
    when(checkoutStatsRepository.findCheckoutStatsWithMaxTotalCheckout()).thenThrow(new RuntimeException("DB error"));

    RuntimeException exception = assertThrows(RuntimeException.class, () -> {
      checkoutStatsService.getAllCheckoutStats(sampleVerifyToken);
    });

    assertEquals("DB error", exception.getMessage());
    verify(verificationService, times(1)).verifyUserToken(sampleVerifyToken);
    verify(checkoutStatsRepository, times(1)).findCheckoutStatsWithMaxTotalCheckout();
  }
}