<template>
<div  v-if="wishListItems.length<=0" id="empty_list">
  <p>{{wishListInfo}}</p>
  <p >
    <img src="https://eazybooks-images.s3.us-east-1.amazonaws.com/empty_wishlist.png" alt="">
  </p>

</div>
   <div class="wish-list-items" v-if="wishListItems.length>0">
    <div v-for="(wishList, index) in wishListItems" :key="index" class="wish-list">
      <div class="wishes">
      <div class="wish-list-title">{{wishList.bookTitle}}</div>
      <div class="remove-list" @click="removeBookFromWishlist(index)"><button>Remove from cart</button></div>
        <div class="wish-list-checkout" @click="addToCheckoutItem(wishList.isbn, index)"><button>Checkout</button></div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { computed, defineComponent,onMounted, ref } from 'vue'
import { useWishlistStore } from '@/stores/useWishlistStore.ts'
import type { WishList } from '@/model/model.ts'
import router from '@/router'
import { useCheckoutItemStore } from '@/stores/useCheckoutItemStore.ts'
import { useAuthStore } from '@/stores/useAuthStore.ts'
import { useLogInStore } from '@/stores/useLogInStore.ts'
  export default defineComponent({
  name: 'WishList',

  setup() {
    const wishListItems = ref<WishList[]>([])
    const wishListInfo = ref(" ")
    const checkoutItemStore = useCheckoutItemStore()

    // let userWishList;
    const wishListStore = useWishlistStore();
    const addToCheckoutItem = (bookIsbn: number, index: number)=>{
      checkoutItemStore.addBookToCheckoutItem(bookIsbn);
      removeBookFromWishlist(index)
      router.push({ path: '/checkout/' })
    };
    const userWishList = computed(() => {

     return  wishListStore.getUserWishList();

    });

    const removeBookFromWishlist = async (index: number): Promise<void> => {
      const adjustedIndex = index < 0 ? wishListItems.value.length + index : index;
      const wishlist = wishListItems.value[adjustedIndex];
      if (wishlist) {
        try {
          await wishListStore.removeBookToWishlist(wishlist); // Wait for API call to finish
          wishListItems.value.splice(index, 1);
          // router.go(0)
        } catch (error) {
          console.log(error);
        }
      }

    }
    onMounted( async () => {
      const authStore = useAuthStore();

      const checkAuth = useLogInStore().checkAuth();

      if (!checkAuth && !authStore.username) {
        await router.push('/')
        return;
      }

      if (!authStore.token) {
         authStore.token = localStorage.getItem("accessToken");
      }
      if (!authStore.username) {
         authStore.username = localStorage.getItem("username");
      }

      if (authStore.token && authStore.username) {
         await wishListStore.getUserWishList();
      }

      if (wishListStore.wishList.length > 0) {
        wishListItems.value = wishListStore.wishList
      } else {
        wishListInfo.value = "Wishlist is empty"
      }
    })

    return {
      userWishList,
      wishListInfo,
      wishListItems,
      removeBookFromWishlist,
      addToCheckoutItem
     };
  }
})
</script>

<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';

html, body {
  padding: 0;
  margin: 0;

  #empty_list{
    margin: 10px auto;
    text-align: center;
    p{
      padding: 10px;
      font-size: 40px;
      color: colors.$accent-color;
    }
    border-radius:0.5rem;
    img{
      max-width: 40%;
    }
  }

  .wish-list-items{
    color: colors.$text-color;
    width: 70%;
     margin: 10px auto;
    padding: 20px;
    border-radius: 0.5rem;
  }
  .wish-list{
    .wishes{
      border-radius: 0.5rem;
      display: flex;
      flex-direction: row;
      margin: 20px auto;
      padding: 10px;
      background: colors.$accent-faint;
      color: colors.$text-color;
      width: 100%;
      .wish-list-title{
        padding:16px;
        font-size: 20px;
        width: 60%;
       }
      .remove-list {
        button{
          font-size: 14px;
          padding: 15px;
          border-radius: 0.5rem;
          border: none;
          outline: none;
          cursor: pointer;
          margin: 10px;
          background: colors.$error-color;
          color: colors.$white-color;
        }
        color: colors.$white-color;
         &:hover {
           color: colors.$text-color;
         }
      }
      .wish-list-checkout {
        button{
          font-size: 14px;
          padding: 15px;
          border-radius: 0.5rem;
          border: none;
          outline: none;
          cursor: pointer;
          margin: 10px;
          background: colors.$text-color;
          color: colors.$white-color;
        }

      }
     }
  }
}
</style>
