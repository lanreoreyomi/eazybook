import { defineStore } from 'pinia'
import axios from 'axios'
import { LOGIN } from '@/api/apis.ts'


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
    statusCode: 0,
    statusText: '',
    isLoggedIn: false,
    loggedInUser: '',
  }),
  actions: {
    async LogIn() {
      try {
        const response = await axios.post<LogInState>(LOGIN, this.userLogIn)
        // Handle successful log in
        localStorage.setItem('accessToken', response.headers['authorization'])
        localStorage.setItem('refreshToken', response.headers['authorization'])
        localStorage.setItem('username', this.userLogIn.username)


        console.log("response: ", response)
        this.$patch({
          statusCode: response.status,
          statusText: String(response.data),
          isLoggedIn: response.status === 200,
        })

      } catch (error) {
        console.log("response: ", error)
        if (axios.isAxiosError(error)) {
          console.log("response: ", error)
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.data || 'Internal Server Error';
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
        }
      }
    },

    setCurrentUser(username: string) {
      this.loggedInUser = username
    },
    checkAuth() {
      const token = localStorage.getItem('accessToken')
      this.isLoggedIn = !!token
      this.loggedInUser = JSON.stringify(localStorage.getItem('username'));
      return this.isLoggedIn
    },
  },
})
