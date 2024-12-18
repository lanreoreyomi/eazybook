// stores/user.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { SIGNUP } from '@/api/apis.ts'

//TODO: Add confirm password field and adjust backend API
interface UserState {
  username: string
  password: string
  firstname: string
  lastname: string
  email: string
}

export const useUserStore = defineStore('user', {
  state: () => ({
    user: {
      username: '',
      password: '',
      firstname: '',
      lastname: '',
      email: '',
    } as UserState, // Define type for user object
    loading: false,
    statusCode: 0,
    statusText: '',
    errorResponse: {
      status: 0,
      message: '',
    },
  }),
  actions: {
    async createUser() {
      this.loading = true
      try {
        const response = await axios.post<UserState>(SIGNUP, this.user)
        // Handle successful user creation
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data), // Ensure statusText is a string
        })
      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          this.$patch({
            statusCode: error.response.status,
            statusText: String(error.response.data), // Ensure statusText is a string
          })
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
  },
})
