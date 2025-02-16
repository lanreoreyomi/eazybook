import { defineStore } from 'pinia'
import axios from 'axios'
import type { BookCatalogue, } from '@/model/model.ts'
import { addBookToCatalogueItem, getCatalogueItemsforUser, removeBookFromCatalogueItem } from '@/api/apis.ts'
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
        const authStore = useAuthStore();
        const response = await axios.get<BookCatalogue[]>(getCatalogueItemsforUser(authStore.username), {
              headers: {
               Authorization: authStore.token
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
      try {
        const response = await axios.post<string>
        (addBookToCatalogueItem(authStore.username, bookIsbn), bookIsbn, {
          headers: {
            Authorization: authStore.token,
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

       try {
         const authStore = useAuthStore();

        const response = await axios.post<string>
        (removeBookFromCatalogueItem(authStore.username, bookIsbn), bookIsbn, {
          headers: {
            Authorization: authStore.token,
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
  },

})
