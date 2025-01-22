// stores/BookCatalogueState.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { addBookToCatalogueWithUsername, BOOKCATALOGUE } from '@/api/apis.ts'
import { accessToken, username } from '@/Utils/AppUtils.ts'
import type { BookCatalogue } from '@/model/model.ts'

interface BookCatalogueState {
  bookCatalogue: BookCatalogue[]; // Array of BookCatalogue
  statusCode: number;
  statusText: string;
}

interface AddBookCatalogueState {
  title: string
  author: string
  isbn: number
  publicationYear: number
  description: string
  available: boolean
  quantityForRent: number

}
export const useBookCatalogueStore = defineStore('bookCatalogue', {
  state: (): BookCatalogueState => ({
    bookCatalogue: [],
    statusCode: 0,
    statusText: '',
  }),
  actions: {
    async getAllBookCatalogues() {
      try {
        const response = await axios.get<BookCatalogue[]>(BOOKCATALOGUE, {
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

export const useAddBookCatalogueStore = defineStore('addBookCatalogue', {
  state: (): { addBookCatalogue: AddBookCatalogueState; statusText: string; statusCode: number } => ({
    addBookCatalogue:{
      title: '',
      author: '',
      isbn: 0,
      publicationYear: 0,
      description: '',
      available: true,
      quantityForRent: 1
    } as AddBookCatalogueState,
    statusText: '',
    statusCode: 0
  }),
  actions: {
    async addBookToCatalogue() {
      try {
        const response = await axios.post<AddBookCatalogueState>(addBookToCatalogueWithUsername(username),
          this.addBookCatalogue, {
            headers: {
              Authorization: accessToken
            }
          })
        // Handle successful user creation
        this.$patch({
          statusCode: response.status,
          statusText : String(response.data)
          // Ensure statusText is a string
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
  }
})
