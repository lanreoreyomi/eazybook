import { createRouter, createWebHistory } from 'vue-router'
import HomeView from '@/views/HomeView.vue'
import CreateAccount from '@/views/CreateAccount.vue'
import LogIn from '@/views/LogIn.vue'

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
   routes: [
    {
      path: '/',
      name: 'home',
      component: HomeView,
    },
    {
      path: '/create-account',
      name: 'create-account',
      component: CreateAccount,
    },
    {
      path: '/login',
      name: 'login',
      component: LogIn,
    },
  ],
})

 export default router
