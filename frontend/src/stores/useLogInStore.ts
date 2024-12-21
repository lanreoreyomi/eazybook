import { defineStore } from 'pinia'
import axios from 'axios'
import { LOGIN } from '@/api/apis.ts'

interface LogInState {
  username: string
  password: string
}

export const useLogInStore = defineStore('user', {
  state: () => ({
    userLogIn: {
      username: '',
      password: '',
    } as LogInState, // Define type for user object
    loading: false,
    statusCode: 0,
    statusText: '',

  }),
  actions: {
    async LogIn() {
      this.loading = true
      try {
        const response = await axios.post<LogInState>(LOGIN, this.userLogIn)
        // Handle successful user creation

        console.log(`response.data: ${JSON.stringify(response.data)}`)
        console.log(`response.status: ${JSON.stringify(response.status)}`)
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
         })
      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          console.log(JSON.stringify(error.response.data))
           this.$patch({
            statusCode: error.response.status,
             statusText: String(error.response.data)
           })
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
  },
})
