// stores/user.ts
import { defineStore } from 'pinia';
import axios from 'axios';
import { SIGNUP } from '@/api/apis.ts'


interface UserState {
  username: string;
  password: string;
  firstname: string;
  lastname: string;
  email: string;
}

export const useUserStore = defineStore('user', {
  state: () => ({
    user: {
      username: '',
      password: '',
      firstname: '',
      lastname: '',
      email: ''
    } as UserState, // Define type for user object
    loading: false,
    error: null
  }),
  actions: {
    async createUser() {
      this.loading = true;
      this.error = null;
      try {
        const response = await axios.post<UserState>(SIGNUP, this.user);
        // Handle successful user creation
        console.log('User created:', response.headers);
      } catch (error) {
         console.error('Error creating user:', error);
      } finally {
        this.loading = false;
      }
    }
  }
});
