package com.eazybooks.bookcatalogue.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.eazybooks.bookcatalogue.model.CheckoutStats;
import com.eazybooks.bookcatalogue.service.IcheckoutStats;
import com.eazybooks.bookcatalogue.service.VerificationService;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;

@ExtendWith(MockitoExtension.class)
class CheckoutStatsControllerTest {
//
//  @InjectMocks
//  private CheckoutStatsController checkoutStatsController;
//
//  @Mock
//  private IcheckoutStats checkoutStatsService;
//
//  @Mock
//  private VerificationService verificationService;
//
//  @Test
//  void getMaxCheckoutOut_Success() {
//    final String validToken = getValidToken();
//    CheckoutStats checkoutStats = new CheckoutStats();
//    checkoutStats.setId("1");
//    checkoutStats.setBookIsbn(123L);
//    checkoutStats.setTotalCheckout(5);
//    checkoutStats.setTitle("Test Title");
//
//    HttpServletRequest request = new MockHttpServletRequest();
//    ResponseEntity<Boolean> verifyToken = ResponseEntity.status(HttpStatus.OK).body(true);
//    request.setAttribute("Authorization", "Bearer "+validToken);
//
//
//    when(verificationService.verifyUserToken(request, null)).thenReturn(verifyToken);
//    when(checkoutStatsService.getAllCheckoutStats()).thenReturn(checkoutStats);
//
//     ResponseEntity<CheckoutStats> response = checkoutStatsController.getMaxCheckoutOut(request);
//
//     assertEquals(HttpStatus.OK, response.getStatusCode());
//    assertNotNull(response.getBody());
//    assertEquals(checkoutStats.getBookIsbn(), response.getBody().getBookIsbn());
//    assertEquals(checkoutStats.getTotalCheckout(), response.getBody().getTotalCheckout());
//    assertEquals(checkoutStats.getTitle(), response.getBody().getTitle());
//
//    verify(checkoutStatsService, times(1)).getAllCheckoutStats();
//    verify(verificationService, times(1)).verifyUserToken(any(), any());
//  }
//
//  @Test
//  void getMaxCheckoutOut_TokenInvalid() {
//    HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer invalid_token");
//
//    ResponseEntity<Boolean> tokenResponse = ResponseEntity.ok(false);
//    when(verificationService.verifyUserToken(request, null)).thenReturn(tokenResponse);
//
//     ResponseEntity<CheckoutStats> response = checkoutStatsController.getMaxCheckoutOut(request);
//
//     assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
//    verify(verificationService, times(1)).verifyUserToken(any(), any());
//  }
//
//  @Test
//  void getMaxCheckoutOut_InternalServerError() {
//     HttpServletRequest request = new MockHttpServletRequest();
//    request.setAttribute("Authorization", "Bearer valid_token");
//
//    when(verificationService.verifyUserToken(request, null)).thenThrow(new RuntimeException("Token Verification Failed"));
//
//     ResponseEntity<CheckoutStats> response = checkoutStatsController.getMaxCheckoutOut(request);
//     assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
//    verify(verificationService, times(1)).verifyUserToken(any(), any());
//  }

  public String getValidToken() {
    return "valid_token";
  }
}
