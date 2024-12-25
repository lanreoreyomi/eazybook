import { defineStore } from 'pinia'
import axios from 'axios'
import { LOGIN } from '@/api/apis.ts'

export let IS_LOGGED_IN = false;
interface LogInState {
  username: string
  password: string
}

export const useLogInStore = defineStore('login', {
  state: () => ({
    userLogIn: {
      username: '',
      password: '',
    } as LogInState,
  //Define type for user object
    loading: false,
    statusCode: 0,
    statusText: '',
    isLoggedIn: false,
  }),
  actions: {
    async LogIn() {
      this.loading = true
      try {
        const response = await axios.post<LogInState>(LOGIN, this.userLogIn)
        // Handle successful log in
          localStorage.setItem('accessToken', response.headers["authorization"]);
          localStorage.setItem('refreshToken', response.headers["authorization"]);

          IS_LOGGED_IN = response.status===200;
          this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
          isLoggedIn: response.status===200,
         })
        console.log(`isLoggedIn: ${this.isLoggedIn}`)
      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          console.log(JSON.stringify(error.response.data))
           this.$patch({
            statusCode: error.response.status,
             statusText: String(error.response.data),
             isLoggedIn: false,
           })
        } else {
          console.log('An unexpected error occurred:', error)
        }
      }
    },
  },
})

