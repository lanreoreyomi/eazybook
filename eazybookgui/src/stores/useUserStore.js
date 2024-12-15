// stores/user.js
import { defineStore } from 'pinia';
import axios from 'axios';
import { SIGNUP } from '@/API/api.js'

export const useUserStore = defineStore('user', {
  state: () => ({
    user: {
      username: '',
      password: '',
      firstname: '',
      lastname: '',
      email: ''
    },
    loading: false,
    error: null
  }),
  actions: {
    async createUser() {
      this.loading = true;
      this.error = null;
      try {
        const response = await axios.post(SIGNUP, this.user);
        // Handle successful user creation
        console.log('User created:', response.headers);
      } catch (error) {
        this.error = error;
        console.error('Error creating user:', error);
      } finally {
        this.loading = false;
      }
    }
  }
});
