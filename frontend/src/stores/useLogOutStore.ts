import axios from 'axios';
import { LOGOUT } from '@/api/apis.ts';
import { defineStore } from 'pinia';
import { accessToken } from '@/Utils/AppUtils.ts';

interface LogOutState {
  loggedOut: '';
}

export const useLogOutStore = defineStore('logout', {
  state: () => ({
    loading: false,
    statusCode: 0,
    statusText: '',
    isLoggedIn: true, // Initialize as logged in (assuming user starts in an authenticated state)
  }),
  actions: {
    async LogOut() {
      try {
        const response = await axios.get<LogOutState>(LOGOUT, {
          headers: {
            Authorization: accessToken
          }
        });

        this.$patch({
          statusCode: response.status,
          isLoggedIn: false,
        });

        localStorage.removeItem('accessToken');
        localStorage.removeItem('refreshToken');

      } catch (error) {
        if (axios.isAxiosError(error) && error.response) {
          console.error(JSON.stringify(error.response.data));
        } else {
          console.error('An unexpected error occurred:', error);
        }
        // Even if logout fails, you might still want to clear tokens and set isLoggedIn to false
        // to prevent the user from accessing protected resources.
        this.isLoggedIn = false;
        localStorage.removeItem('accessToken');
        localStorage.removeItem('refreshToken');
      }
    },

    checkAuth() {
      // 1. Check for a valid authentication token (e.g., in local storage)
      const token = localStorage.getItem('accessToken');

      // 2. Update the login status based on the token's presence and validity
      this.isLoggedIn = token ? true : false; // You might need more complex validation here
    }
  },
});
