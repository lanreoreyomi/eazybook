// stores/BookCatalogueState.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { GETALLBOOKCATALOGUE } from '@/api/apis.ts'
import { accessToken } from '@/Utils/AppUtils.ts'
import type { BookCatalogue } from '@/model/model.ts'

interface BookCatalogueState {
  bookCatalogue: BookCatalogue[]; // Array of BookCatalogue
  loading: boolean;
  statusCode: number;
  statusText: string;
}

export const useBookCatalogueStore = defineStore('bookCatalogue', {
  state: (): BookCatalogueState => ({
    bookCatalogue: [],
    loading: false,
    statusCode: 0,
    statusText: '',
  }),
  actions: {
    async getAllBookCatalogues() {
       try {
        const response = await axios.get<BookCatalogue[]>(GETALLBOOKCATALOGUE, {
          headers: {
            Authorization: accessToken
          }
        })
        // Handle successful user creation
         this.$patch({
          statusCode: response.status,
          bookCatalogue : response.data
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
    }
  }
})
