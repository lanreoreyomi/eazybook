<template>
<h2 v-if="wishListInfo" id="empty_list">{{wishListInfo}}</h2>
   <div class="wish-list-items" v-if="wishListItems.length>0">
    <div v-for="(wishList, index) in wishListItems" :key="index" class="wish-list">
      <div class="wishes">
      <div class="wish-list-title">{{wishList.bookTitle}}</div>
      <div class="remove-list" @click="removeBookFromWishlist(index)"><button>Remove from cart</button></div>
        <div class="wish-list-checkout"><button>Checkout</button></div>
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { computed, defineComponent, onBeforeMount, ref } from 'vue'
import { useWishlistStore } from '@/stores/useWishlistStore.ts'
import type { WishList } from '@/model/model.ts'
import router from '@/router'
  export default defineComponent({
  name: 'WishList',

  setup() {
    const wishListItems = ref<WishList[]>([])
    const wishListInfo = ref(" ")

    // let userWishList;
    const wishListStore = useWishlistStore();

    const userWishList = computed(() => {
      console.log(wishListStore.getUserWishList())
      return wishListStore.getUserWishList();
    });

    const removeBookFromWishlist = (index: number): void => {
      const wishlist = wishListItems.value.at(index);
      if (wishlist) {
        wishListStore.removeBookToWishlist(wishlist);
        router.go(0)
      }
    }
    onBeforeMount(async () => {
      await wishListStore.getUserWishList()
      if (wishListStore.wishList.length>0) {
        wishListItems.value = wishListStore.wishList
      }else{
        wishListInfo.value = "Wishlist is empty"
      }
    })
    return {
      userWishList,
      wishListInfo,
      wishListItems,
      removeBookFromWishlist,
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
    margin: 40px auto;
    padding: 10px;
    font-size: 40px;
    text-align: center;
    color: colors.$accent-color;

    width: 60%;
    border-radius:0.5rem;
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
