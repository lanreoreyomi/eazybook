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
        target: 'http://eazybooks.com:9084',
        changeOrigin: true
      },
      '/auth/login': {
        target: 'http://eazybooks.com:9084',
        changeOrigin: true
      },
      '/auth/logout': {
        target: 'http://eazybooks.com:9084',
        changeOrigin: true
      },
      '/bookcatalogue': {
        target: 'http://eazybooks.com:9089',
        changeOrigin: true
      },
      '/wishlist/': {
        target: 'http://eazybooks.com:8017',
        changeOrigin: true
      },
      '/checkoutitems/': {
        target: 'http://eazybooks.com:9089',
        changeOrigin: true
      }
    }
  }
})
