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
        if (axios.isAxiosError(error) && error.response) {
          console.error(JSON.stringify(error.response.data));
        } else {
          console.error('An unexpected error occurred:', error);
        }

        localStorage.removeItem('accessToken');
        localStorage.removeItem('refreshToken');
      }

    },

},

});
