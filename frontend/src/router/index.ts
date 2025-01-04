import { createRouter, createWebHistory } from 'vue-router'
import HomeView from '@/views/HomeView.vue'
import CreateAccount from '@/views/CreateAccount.vue'
import LogIn from '@/views/LogIn.vue'
import CatalogueView from '@/views/CatalogueView.vue'
import Profile from '@/views/Profile.vue'
import WishList from '@/views/WishList.vue'
import BorrowHistory from '@/views/BorrowHistory.vue'

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
     {
       path: '/catalogue',
       name: 'catalogue',
       component: CatalogueView,
     },
     {
       path: '/profile',
       name: 'profile',
       component: Profile,
     },
     {
       path: '/wishlist',
       name: 'wishlist',
       component: WishList,
     },
     {
       path: '/borrowhistory',
       name: 'borrowhistory',
       component: BorrowHistory,
     },
  ],
})

 export default router
