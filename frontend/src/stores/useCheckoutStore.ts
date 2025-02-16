import { defineStore } from 'pinia'
import axios from 'axios'
 import {
  getAllCheckoutForUser,
  processCheckout, returnCheckout
} from '@/api/apis.ts'
import type { CheckedOutHistory } from '@/model/model.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'


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
      const authStore = useAuthStore();
      try {
        const response =
          await axios.post<string>(processCheckout(authStore.username, bookIsbn), null, {
          headers: {
            Authorization: authStore.token,
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
      const authStore = useAuthStore();

      try {
        const response =
          await axios.post<string>(returnCheckout(authStore.username, bookIsbn), null, {
            headers: {
              Authorization: authStore.token
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
    const authStore = useAuthStore();
    try {
        const response = await axios.get<CheckedOutHistory[]>
        (getAllCheckoutForUser(authStore.username), {
        headers: {
          Authorization: authStore.token,
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
