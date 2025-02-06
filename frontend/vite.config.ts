import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import vueDevTools from 'vite-plugin-vue-devtools'

// https://vite.dev/config/
export default defineConfig({
  plugins: [
    vue(),
    vueDevTools(),
  ],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url))
    },
  },
  server: {
    proxy: {
      '/auth/create-account': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/auth/login': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/auth/logout': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/bookcatalogue/': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/stats/': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/wishlist/': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/checkoutitems/': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      },
      '/checkout/': {
        target: 'http://ec2-52-91-196-70.compute-1.amazonaws.com',
        changeOrigin: true
      }
    }
  }
})
