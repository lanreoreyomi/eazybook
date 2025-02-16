// stores/user.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import type { BookCatalogue, WishList } from '@/model/model.ts'
 import {
  addBookToWishlistWithUsername,
  getWishListForUser,
  removeBookFromWishlistWithUsername
} from '@/api/apis.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'

interface WishlistState {
  username: string;
  wishList: WishList[];
  statusCode: number,
  statusText: string,
}
interface BookCatalogueState {
  bookCatalogue: BookCatalogue[];
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
      const authstore = useAuthStore()
       // Access loggedInUser *after* ensuring the user is logged in

      try {
        const response =
          await axios.get<WishList[]>(getWishListForUser(String(authstore.username)), {
            headers: {
              Authorization: authstore.token,
            }
          })

        this.$patch({
          statusCode: response.status,
          wishList: response.data, // Ensure statusText is a string
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

    async addBookToWishlist(book: BookCatalogue) {
      const authstore = useAuthStore()

       try {
        const response = await axios.post<BookCatalogueState>
        (addBookToWishlistWithUsername(authstore.username), book.isbn, {
          headers: {
            Authorization: authstore.token,
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

      const authstore = useAuthStore();

      try {
        const response = await axios.post<BookCatalogueState>
        (removeBookFromWishlistWithUsername(authstore.username), wishList.isbn, {
          headers: {
            Authorization: authstore.token,
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
          this.statusText = error.response?.data || 'Internal Server Error';
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
