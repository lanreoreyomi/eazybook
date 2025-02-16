// stores/BookCatalogueState.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { addBookToCatalogueWithUsername, BOOKCATALOGUE } from '@/api/apis.ts'
import type { BookCatalogue } from '@/model/model.ts'
import router from '@/router'
import { useAuthStore } from '@/stores/useAuthStore.ts'

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

      const authStore = useAuthStore()

      try {
        const response = await axios.get<BookCatalogue[]>(BOOKCATALOGUE, {
          headers: {
            Authorization: authStore.token
          }
        })

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
      const authStore = useAuthStore()

      try {
        const response = await axios.post<AddBookCatalogueState>
        (addBookToCatalogueWithUsername(authStore.username),
          this.addBookCatalogue, {
          headers: {
            Authorization: authStore.token,
          }
        })
        // Handle successful user creation
        this.$patch({
          statusCode: response.status,
          statusText : String(response.data)
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

