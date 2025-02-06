import { defineStore } from 'pinia'
import axios from 'axios'
import { accessToken, username } from '@/Utils/AppUtils.ts'
import {
  api,
  getAllCheckoutForUser,
  processCheckout, returnCheckout
} from '@/api/apis.ts'
import type { CheckedOutHistory } from '@/model/model.ts'


interface CheckoutState {
  statusCode: number;
  statusText: string;
}

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
          await api.post<string>(processCheckout(username, bookIsbn), null, {
          headers: {
            Authorization: accessToken
          }
        })
          this.$patch({
          statusCode: response.status,
          statusText: response.data, // Ensure statusText is a string
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
    async returnBook(bookIsbn: number) {
      try {
        const response =
          await api.post<string>(returnCheckout(username, bookIsbn), null, {
            headers: {
              Authorization: accessToken
            }
          })
        console.log(response.status)
        console.log(response.statusText)
        console.log(response.data)
        this.$patch({
          statusCode: response.status,
          statusText: response.data, // Ensure statusText is a string
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
        const response = await api.get<CheckedOutHistory[]>(getAllCheckoutForUser(username), {
        headers: {
          Authorization: accessToken
        }
      })
      this.$patch({
        statusCode: response.status,
        checkedOutHistory: response.data
      })
    } catch (error) {
      if (axios.isAxiosError(error)) {
        this.statusCode = error.response?.status || 500;
        this.statusText = error.response?.data || 'Book already checked out';
      } else {
        this.statusCode = 500;
        this.statusText = 'An unexpected error occurred';
      }
    }
  },
}
})
