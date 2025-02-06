import { defineStore } from 'pinia'
import axios from 'axios'
import type { BookCatalogue, } from '@/model/model.ts'
import { accessToken, username } from '@/Utils/AppUtils.ts'
import { addBookToCatalogueItem, api, getCatalogueItemsforUser, removeBookFromCatalogueItem } from '@/api/apis.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'


//for getting all catalgues
interface BookCatalogueState {
  checkoutItems: BookCatalogue[]; // Array of BookCatalogue
  statusCode: number;
  statusText: string;
}
export const useCheckoutItemStore = defineStore('checkoutItem', {
  state: (): BookCatalogueState => ({
    checkoutItems: [],
    statusCode: 0,
    statusText: '',
  }),
  actions: {
    async getCheckoutItemsforUser() {
      try {
        const response = await api.get<BookCatalogue[]>(getCatalogueItemsforUser(username), {
              headers: {
               Authorization: accessToken
              }
        })
        this.$patch({
          statusCode: response.status,
          checkoutItems: response.data,
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
    async addBookToCheckoutItem(bookIsbn: number) {

      const authStore = useAuthStore()

      const username = String(authStore.username)

      try {
        const response = await api.post<string>
        (addBookToCatalogueItem(username, bookIsbn), bookIsbn, {
          headers: {
            Authorization: accessToken,
          }
        })
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
        })

      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          this.$patch({
            statusCode: error.response.status,
            statusText: String(error.response.data),
          })
        }
      }
    },
    async removeBookFromCheckoutItem(bookIsbn: number) {
      const username = localStorage.getItem('username')
      try {

        const response = await api.post<string>
        (removeBookFromCatalogueItem(username, bookIsbn), bookIsbn)
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
  },

})
