import { defineStore } from 'pinia'
import axios from 'axios'
import type { BookCatalogue, } from '@/model/model.ts'
import { accessToken, username } from '@/Utils/AppUtils.ts'
import { addBookToCatalogueItem, getCatalogueItemsforUser, removeBookFromCatalogueItem } from '@/api/apis.ts'


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
    async getCatalogueItemsforUser() {
      try {
        const response = await axios.get<BookCatalogue[]>(getCatalogueItemsforUser(username), {
              headers: {
               Authorization: accessToken
              }
        })
        this.$patch({
          statusCode: response.status,
          checkoutItems: response.data, // Ensure statusText is a string
        })
      } catch (error) {
        console.error('Error fetching checkout stats:', error);
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

      const username = localStorage.getItem('username')
      try {

        const response = await axios.post<string>
        (addBookToCatalogueItem(username, bookIsbn), bookIsbn, {
          headers: {
            Authorization: accessToken,
            'Content-Type': 'application/json'
          }
        })
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
          console.log("this.statusCode", this.statusCode)
          console.log("response.statusText", this.statusText)
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
    async removeBookFromCheckoutItem(bookIsbn: number) {
      const username = localStorage.getItem('username')
      try {

        const response = await axios.post<string>
        (removeBookFromCatalogueItem(username, bookIsbn), bookIsbn, {
          headers: {
            Authorization: accessToken,
            'Content-Type': 'application/json'
          }
        })
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
        })

      } catch (error) {
        console.error('Error fetching data:', error);
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
