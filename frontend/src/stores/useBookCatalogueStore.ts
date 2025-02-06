// stores/BookCatalogueState.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { addBookToCatalogueWithUsername, api, BOOKCATALOGUE } from '@/api/apis.ts'
import { accessToken, username } from '@/Utils/AppUtils.ts'
import type { BookCatalogue } from '@/model/model.ts'
import router from '@/router'

interface BookCatalogueState {
  bookCatalogue: BookCatalogue[]; // Array of BookCatalogue
  statusCode: number;
  statusText: string;
  isLoading: boolean, // Add loading state

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
    isLoading: false, // Add loading state
  }),
  actions: {
    async getAllBookCatalogues() {
      this.isLoading = true;

      try {
        const response = await api.get<BookCatalogue[]>(BOOKCATALOGUE)

        this.$patch({
          statusCode: response.status,
          bookCatalogue: response.data,
          isLoading: false,
        })
      } catch (error) {
        if (axios.isAxiosError(error)) {
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.data || 'Internal Server Error';
          this.isLoading = false;
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
          this.isLoading = false;
        }

      }
    }
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
        const response = await api.post<AddBookCatalogueState>(addBookToCatalogueWithUsername(username),
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
      router.go(0);
    },
  }
})

