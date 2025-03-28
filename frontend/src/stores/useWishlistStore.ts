// stores/user.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import type { BookCatalogue, WishList } from '@/model/model.ts'
 import { addBookToWishlistWithUsername, getWishListForUser, removeBookFromWishlistWithUsername } from '@/api/apis.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'

interface WishlistState {
  username: string;
  wishList: WishList[];
  statusCode: number,
  statusText: string,
}
interface BookCatalogueState {
  bookCatalogue: BookCatalogue[]; // Array of BookCatalogue
  statusCode: number;
  statusText: string;
}
export const useWishlistStore = defineStore('wishlist', {
  state: (): WishlistState => ({
    username: '',
    wishList: [],
    statusCode: 0,
    statusText: '',
  }),

  actions: {
    async getUserWishList() {
      // Access loggedInUser *after* ensuring the user is logged in
      const authStore = useAuthStore();

      try {
        const response =
          await axios.get<WishList[]>(getWishListForUser(authStore.username), {
            headers: {
              Authorization: authStore.token,
            }
          })
        this.$patch({
          statusCode: response.status,
          wishList: response.data, // Ensure statusText is a string
        })
      } catch (error) {
        if (axios.isAxiosError(error)) {
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.statusText || 'Internal Server Error';
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
        }
      }
    },
    setCurrentUser(username: string) {
      this.username = username;
    },
    async addBookToWishlist(book: BookCatalogue) {

      try {
        const authStore = useAuthStore();

        const response = await axios.post<BookCatalogueState>
        (addBookToWishlistWithUsername(authStore.username), book.isbn, {
          headers: {
            Authorization: authStore.token,
            'Content-Type': 'application/json'
          }
        })
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
        })

      } catch (error) {
        if (axios.isAxiosError(error)) {
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.data || 'Internal Server Error';
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
        }
      }
    },
    async removeBookToWishlist(wishList: WishList) {

      const authStore = useAuthStore();

      try {
        const response = await axios.post<BookCatalogueState>
        (removeBookFromWishlistWithUsername(authStore.username), wishList.isbn, {
          headers: {
            Authorization: authStore.token,
            'Content-Type': 'application/json'
          }
        })

        this.$patch({
          statusCode: response.status,
          statusText: String(response.statusText),
        })

      } catch (error) {
        if (axios.isAxiosError(error)) {
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.statusText || 'Internal Server Error';
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
        }
      }
    },
  },

  getters: {
    getCurrentUser: (state) => state.username
  },
})
