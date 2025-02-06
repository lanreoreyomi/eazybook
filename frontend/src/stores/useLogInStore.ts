import { defineStore } from 'pinia'
import axios from 'axios'
import { LOGIN } from '@/api/apis.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'


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

       const authStore = useAuthStore();
       try {
         const response = await axios.post<LogInState>(LOGIN, this.userLogIn)

         authStore.token = response.headers['authorization'];
         authStore.username = this.userLogIn.username;

         localStorage.setItem('accessToken', response.headers['authorization'])
         localStorage.setItem('username', this.userLogIn.username)

         this.$patch({
           statusCode: response.status,
           isLoggedIn: response.status === 200,
         })

       } catch (error) {
         if (axios.isAxiosError(error)) {
           this.statusCode = error.response?.status || 500;
           this.statusText = "Error signing user in.";
         } else {
           this.statusCode = 500;
           this.statusText = 'An unexpected error occurred';
         }
       }
     },

     checkAuth() {
       const token = localStorage.getItem('accessToken')
       this.isLoggedIn = !!token
       this.loggedInUser = JSON.stringify(localStorage.getItem('username'));
       return this.isLoggedIn
     },
   },
 })

