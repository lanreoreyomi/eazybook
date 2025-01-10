import { defineStore } from 'pinia'
import axios from 'axios'
import { accessToken, username } from '@/Utils/AppUtils.ts'
import {
  getAllCheckoutForUser,

  // addBookToCatalogueItem,
  processCheckout, returnCheckout
  // removeBookFromCatalogueItem
} from '@/api/apis.ts'
import type { CheckedOutHistory } from '@/model/model.ts'


//for getting all catalgues
interface CheckoutState {
  statusCode: number;
  statusText: string;
}//for getting all catalgues

interface CheckoutInfoState {
  checkedOutHistory: CheckedOutHistory[]
  statusCode: number,
  statusText: string,
}

export const useCheckoutStore = defineStore('checkout', {
  state: (): CheckoutState => ({
    statusCode: 0,
    statusText: '',
  }),
  actions: {
    async checkBookOut(bookIsbn: number) {
        try {
        const response =
          await axios.post<string>(processCheckout(username, bookIsbn), null, {
          headers: {
            Authorization: accessToken
          }
        })
          this.$patch({
          statusCode: response.status,
          statusText: response.data, // Ensure statusText is a string
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
    async returnBook(bookIsbn: number) {
      try {
        const response =
          await axios.post<string>(returnCheckout(username, bookIsbn), null, {
            headers: {
              Authorization: accessToken
            }
          })

        console.log("response.data", response.data)
        console.log("response.status", response.status)
        console.log(response.statusText, response.statusText)
        this.$patch({
          statusCode: response.status,
          statusText: response.data, // Ensure statusText is a string
        })
      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          this.$patch({
            statusCode: error.response.status,
            statusText: String(error.response.data), // Ensure statusText is a string
          })
          console.log("response.data", error.response.data)
          console.log("response.status", error.response.status)
          console.log("response.statusText", error.response.statusText)
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
  },

})

export const useCheckedOutHistory = defineStore('CheckedOutHistory', {
  state: (): CheckoutInfoState  => ({
    checkedOutHistory: [],
      statusCode: 0,
      statusText: ''
  }),
actions: {
  async getAllCheckoutHistoryForUser() {
    try {
        const response = await axios.get<CheckedOutHistory[]>(getAllCheckoutForUser(username), {
        headers: {
          Authorization: accessToken
        }
      })
    // Handle successful user creation
      this.$patch({
        statusCode: response.status,
        checkedOutHistory: response.data
        // Ensure statusText is a string
      })
    } catch (error) {
      if (axios.isAxiosError(error) && error.response) {
        this.$patch({
          statusCode: error.response.status
        })
      } else {
        console.log('An unexpected error occurred:', error)
      }
    }
  },

}
})
