<template>

  <footer>
    <div class="container">
      <div class="footer-content">
        <div class="footer-section">
          <h3>About Us</h3>
          <p>Eazybook is an online library that provides convenient access to a vast collection of books, audiobooks, and other digital content. Explore our extensive catalog, borrow titles with ease, and enjoy reading anytime, anywhere.

          </p>
        </div>
        <div class="footer-section">
          <h3>Quick Links</h3>
          <nav>
            <ul>
              <li>
                <router-link to="/">Home</router-link>
              </li>
              <li >
                <router-link to="/bookcatalogue/">Catalogue</router-link>
              </li>

              <li >
                <router-link to="/wishlist/">WishList</router-link>
              </li>
              <li >
                <router-link to="/checkout/"> Checkout</router-link>
              </li>

            </ul>
          </nav>
        </div>
        <div class="footer-section">
          <h3>Contact Us</h3>
          <p>123 Main Street<br>Ottawa, ON 12345</p>
          <p>Email: lanreorey@gmail.com<br>Phone: (613) 300 1233</p>
          <div class="social-icons">
            <a href="#"><i class="fab fa-facebook-f"></i></a>
            <a href="#"><i class="fab fa-twitter"></i></a>
            <a href="#"><i class="fab fa-instagram"></i></a>
            <a href="#"><i class="fab fa-linkedin-in"></i></a>
          </div>
        </div>
      </div>
      <div class="copyright">
        <p>&copy; 2025 Eazybooks. All rights reserved.</p>
      </div>
    </div>
  </footer>
</template>

<script  lang="ts">
export default {
  name: 'FooterComponent',
}
</script>


<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';


body {
  display: flex;
  min-height: 100vh;
  flex-direction: column;
}.content-wrapper { /* Add a wrapper for your main content */
   flex: 1;
 }
.container {
   width: 100%;
   margin: 0 auto;
   padding: 20px;
 }

/* Footer Styling */
footer {
  width: 100%;

  min-height: 20vh;
  background-color: colors.$text-color;
  color: colors.$white-color;
  padding: 60px 0;
}.footer-content {
   display: flex;
   flex-wrap: wrap;
   justify-content: space-between;

  h3{
    font-size: 36px;
  }
 }.footer-section {
    width: 30%;
    margin-bottom: 20px;

    h3 {
      color: colors.$accent-color;
      margin-bottom: 10px;
      font-weight: bolder;
    }

    ul {
      list-style: none;
      padding: 0;

      li {
        a {
          color: #ddd;
          text-decoration: none;
          transition: color 0.3s ease;

        }
      }
    }
  }.social-icons {
     margin-top: 20px;

     a {
       display: inline-block;
       margin-right: 10px;
       font-size: 20px;
       color: #ddd;
       transition: color 0.3s ease;

       &:hover {
         color: #fff;
       }
     }
   }

.copyright {
    text-align: center;
    font-size: 14px;
    margin-top: 40px;
    }
</style>
