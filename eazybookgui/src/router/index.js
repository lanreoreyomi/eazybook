import { createRouter, createWebHistory } from 'vue-router'
import HomeView from '@/views/home-view.vue'
import CreateAccount from '@/views/sign-up.vue'

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes: [
    {
      path: '/',
      name: 'home',
      component: HomeView,
    },
    {
      path: '/signup',
      name: 'signup',
      component: CreateAccount,
    },
  ],
})

export default router
