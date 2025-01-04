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

         this.$patch({
           statusCode: response.status,
           statusText: String(response.data),
           isLoggedIn: response.status === 200,
         })

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

