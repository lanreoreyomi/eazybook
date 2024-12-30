import './assets/main.css'
import './assets/scss/_colors.scss' // Import your global styles

import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { createVfm } from 'vue-final-modal'
import 'vue-final-modal/style.css'
  /* import font awesome icon component */

import App from './App.vue'
import router from './router'

const app = createApp(App)
const vfm = createVfm()

app.use(createPinia())
app.use(router)
app.use(vfm)
app.mount('#app')
