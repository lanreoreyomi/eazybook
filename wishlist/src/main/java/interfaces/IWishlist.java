package interfaces;

import com.eazybooks.wishlist.model.Wishlist;
import java.util.List;

public interface IWishlist {

  Wishlist save(Wishlist wishlist);
  Wishlist findById(Long id);
  List<Wishlist> findByUserName(String username);
  Wishlist findByBookIsbnAndUsername(Long isbn, String username);
  Wishlist findByBookIsbn(Long bookIsbn);
  void deleteById(Long id);
  void removeByBookIsbn(Long bookIsbn);
}
