package interfaces;

import com.eazybooks.wishlist.DTO.VerifyToken;
import com.eazybooks.wishlist.exceptions.AuthorizationHeaderNotFound;
import com.eazybooks.wishlist.exceptions.BookExistException;
import com.eazybooks.wishlist.exceptions.BookNotFoundException;
import com.eazybooks.wishlist.model.CreateWishListRequest;
import com.eazybooks.wishlist.model.Wishlist;
import java.util.List;

public interface IWishlist {

  String adBookToWishlist(VerifyToken verifyTokenRequest, CreateWishListRequest wishlist)
      throws AuthorizationHeaderNotFound, BookNotFoundException, BookExistException;
  Wishlist findById(Long id);
  List<Wishlist> findByUserName(VerifyToken verifyTokenRequest) throws AuthorizationHeaderNotFound;
  Wishlist findByBookIsbnAndUsername(VerifyToken verifyTokenRequest, Long bookIsbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException;
  Wishlist findByBookIsbn(Long bookIsbn);
  void deleteById(Long id);
  String removeByBookIsbn(VerifyToken verifyTokenRequest, Long bookIsbn)
      throws AuthorizationHeaderNotFound, BookNotFoundException;
}
