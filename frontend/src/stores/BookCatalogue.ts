// stores/BookCatalogueState.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { GETALLBOOKCATALOGUE } from '@/api/apis.ts'
import { accessToken } from '@/Utils/AppUtils.ts'

interface BookCatalogueState {
  title: string
  author: string
  isbn: string
  publicationYear: string
  description: string
  isAvailable: boolean
  quantityForRent: number

}

let BookCatalogueState
export const useBookCatalogueStore = defineStore('bookCatalogue', {
  state: () => ({
    bookCatalogue: {
      title: '',
      author: '',
      isbn: '',
      publicationYear: '',
      description: '',
      isAvailable: false,
      quantityForRent: 0
    } as BookCatalogueState, // Define type for user object
    loading: false,
    statusCode: 0,
    errorResponse: {
      status: 0,
      message: ''
    }
  }),
  actions: {
    async getAllBookCatalogues() {
      this.loading = true

      try {
        const response = await axios.get<BookCatalogueState>(GETALLBOOKCATALOGUE, {
          headers: {
            Authorization: accessToken
          }
        })
        // Handle successful user creation

        this.$patch({
          statusCode: response.status,
          bookCatalogue: response.data // Ensure statusText is a string
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
