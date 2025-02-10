import { fileURLToPath, URL } from 'node:url'

import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import vueDevTools from 'vite-plugin-vue-devtools'

 // const backEndUrl = 'http://ec2-34-227-14-199.compute-1.amazonaws.com';

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
  // server: {
  //   proxy: {
  //     '/auth/create-account': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/auth/login': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/auth/logout': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/bookcatalogue/': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/stats/': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/wishlist/': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/checkoutitems/': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     },
  //     '/checkout/': {
  //       target: backEndUrl,
  //       changeOrigin: true
  //     }
  //   }
  // }
})
