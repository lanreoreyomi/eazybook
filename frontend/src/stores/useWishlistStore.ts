// stores/user.ts
import { defineStore } from 'pinia'
import axios from 'axios'
 import type { WishList } from '@/model/model.ts'
import { accessToken } from '@/Utils/AppUtils.ts'
import { getWishListForUser } from '@/api/apis.ts'

interface WishlistState {
  username: string;
  wishList: WishList[];
  statusCode: number,
  statusText: string,
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
        // Handle successful user creation
         console.log(`Response data: ${JSON.stringify(response.data)}`);
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data), // Ensure statusText is a string
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
      console.log("Recieved loggedInUser from wislist store", username)
      this.username = username;

      console.log("this.username", this.username);
    }
  },
  getters: {
      getCurrentUser: (state) => state.username
  },
})
