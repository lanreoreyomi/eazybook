import { createRouter, createWebHashHistory } from 'vue-router'
import HomeView from '@/views/HomeView.vue'
import CreateAccount from '@/views/CreateAccount.vue'
import LogIn from '@/views/LogIn.vue'
import CatalogueView from '@/views/CatalogueView.vue'
 import WishList from '@/views/WishList.vue'
 import Checkout from '@/views/Checkout.vue'

const router = createRouter({
  history: createWebHashHistory(import.meta.env.BASE_URL),
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
       path: '/bookcatalogue/',
       name: 'bookcatalogue',
       component: CatalogueView,
     },

     {
       path: '/wishlist/',
       name: 'wishlist',
       component: WishList,
     },
     {
       path: '/checkout/',
       name: 'checkout',
       component: Checkout,
     },
  ]
})

 export default router
