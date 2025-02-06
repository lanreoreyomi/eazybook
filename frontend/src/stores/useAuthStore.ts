import { defineStore } from 'pinia'


export const useAuthStore = defineStore('auth', {
  state: () => ({
    token: localStorage.getItem('accessToken') || null,
    username: localStorage.getItem('username') || null,
  }),

});
