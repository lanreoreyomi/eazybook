import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import vueDevTools from 'vite-plugin-vue-devtools'

const reverseProxyHost = 'http://localhost'

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
        target: `${reverseProxyHost}:9084`,
        changeOrigin: true
      },
      '/auth/login': {
        target: `${reverseProxyHost}:9084`,
        changeOrigin: true
      },
      '/auth/logout/': {
        target: `${reverseProxyHost}:9084`,
        changeOrigin: true
      },
      '/bookcatalogue/': {
        target: `${reverseProxyHost}:9189`,
        changeOrigin: true
      },
      '/stats/': {
        target: `${reverseProxyHost}:9189`,
        changeOrigin: true
      },
      '/wishlist/': {
        target: `${reverseProxyHost}:8017`,
        changeOrigin: true
      },
      '/checkoutitems/': {
        target: `${reverseProxyHost}:9189`,
        changeOrigin: true
      },
      '/checkout/': {
        target: `${reverseProxyHost}:9189`,
        changeOrigin: true
      }
    }
  }
})
