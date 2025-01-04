// stores/user.ts
import { defineStore } from 'pinia'
import axios from 'axios'
 import type { BookCatalogue, WishList } from '@/model/model.ts'
import { accessToken } from '@/Utils/AppUtils.ts'
import { addBookToWishlistWithUsername, getWishListForUser, removeBookFromWishlistWithUsername } from '@/api/apis.ts'

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
       const username = localStorage.getItem('username')
      // Access loggedInUser *after* ensuring the user is logged in
      try {
        const response =
          await axios.get<WishList[]>(getWishListForUser(username), {
            headers: {
              Authorization: accessToken
            }
          })
          console.log(`Response data: ${JSON.stringify(response.data)}`);
        this.$patch({
          statusCode: response.status,
          wishList: response.data, // Ensure statusText is a string
        })
      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          this.$patch({
            statusCode: error.response.status,
            statusText: String(error.response.data), // Ensure statusText is a string
          })
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
    setCurrentUser(username: string) {
       this.username = username;
     },
    async addBookToWishlist(book: BookCatalogue) {

       const username = localStorage.getItem('username')
      try {

        const response = await axios.post<BookCatalogueState>
        (addBookToWishlistWithUsername(username), book.isbn, {
          headers: {
            Authorization: accessToken,
            'Content-Type': 'application/json'
          }
        })
        // console.log(`Response data: ${JSON.stringify(response)}`);


        this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
        })

      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          console.log(JSON.stringify(error.response.data))
          this.$patch({
            statusCode: error.response.status,
            statusText: String(error.response.data),
          })
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },

    async removeBookToWishlist(wishList: WishList) {

      const username = localStorage.getItem('username')
      try {
        const response = await axios.post<BookCatalogueState>
        (removeBookFromWishlistWithUsername(username), wishList.isbn, {
          headers: {
            Authorization: accessToken,
            'Content-Type': 'application/json'
          }
        })

        this.$patch({
          statusCode: response.status,
          statusText: String(response.statusText),
        })

      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          console.log(JSON.stringify(error.response.data))
          this.$patch({
            statusCode: error.response.status,
            statusText: String(error.response.data),
          })
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
  },

  getters: {
      getCurrentUser: (state) => state.username
  },
})
