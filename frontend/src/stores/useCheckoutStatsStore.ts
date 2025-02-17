import type { CheckoutStats } from '@/model/model.ts'
import { defineStore } from 'pinia'
import axios from 'axios'
 import { CHECKOUTSTATS } from '@/api/apis.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'

interface stats {
  stats: CheckoutStats | null,
  statusCode: number,
  statusText: string,
}


export const useCheckoutStatsStore
  = defineStore("checkoutStats",{
  state: (): stats => ({
    stats: null,
    statusCode: 0,
    statusText: '',
  }),

  actions: {
    async fetchCheckoutStats (): Promise<void> {
      try {
        const authStore = useAuthStore();

        const response = await axios.get<CheckoutStats>(CHECKOUTSTATS, {
          headers: {
            Authorization: authStore.token,
          }
        })
        // Handle successful user creation
        this.$patch({
          stats: response.data,
          statusCode : response.status
        })
      }catch (error) {
        if (axios.isAxiosError(error)) {
          this.statusCode = error.response?.status || 500;
          this.statusText = error.response?.data || 'Internal Server Error';
        } else {
          this.statusCode = 500;
          this.statusText = 'An unexpected error occurred';
        }
      }
    }
  }
})
