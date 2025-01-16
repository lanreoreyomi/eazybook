// stores/user.ts
import { defineStore } from 'pinia'
import axios from 'axios'
import { CREATEACCOUUNT } from '@/api/apis.ts'

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
    statusCode: 0,
    statusText: '',
    errorResponse: {
      status: 0,
      message: '',
    },
  }),
  actions: {
    async createUser() {
      try {
        const response = await axios.post<UserState>(CREATEACCOUUNT, this.user)
        // Handle successful user creation
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data), // Ensure statusText is a string
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
