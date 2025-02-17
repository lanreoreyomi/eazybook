import { defineStore } from 'pinia'
import axios from 'axios'
 import {
  getAllCheckoutForUser,
  // addBookToCatalogueItem,
  processCheckout, returnCheckout
  // removeBookFromCatalogueItem
} from '@/api/apis.ts'
import type { CheckedOutHistory } from '@/model/model.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'


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
          this.statusText = error.response?.statusText || 'Internal Server Error';
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
        const response =
          await axios.get<CheckedOutHistory[]>(getAllCheckoutForUser(authStore.username), {
          headers: {
            Authorization: authStore.token,
          }
        })
        // Handle successful user creation
        this.$patch({
          statusCode: response.status,
          checkedOutHistory: response.data
          // Ensure statusText is a string
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
  }
})
