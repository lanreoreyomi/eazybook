import axios from 'axios';
import { LOGOUT } from '@/api/apis.ts';
import { defineStore } from 'pinia';
import { accessToken } from '@/Utils/AppUtils.ts';

interface LogOutState {
  loggedOut: '';
}

export const useLogOutStore = defineStore('logout', {
  state: () => ({
    statusCode: 0,
    statusText: '',
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
        });

        localStorage.removeItem('accessToken');
        localStorage.removeItem('refreshToken');
        localStorage.removeItem('username');

      } catch (error) {
        console.error('Error fetching checkout stats:', error);
        if (axios.isAxiosError(error)) {
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.data || 'Internal Server Error';
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
        }
        localStorage.removeItem('accessToken');
        localStorage.removeItem('refreshToken');
      }
    },

},

});
