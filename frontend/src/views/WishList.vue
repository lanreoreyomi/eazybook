<template>
<h2>WishList</h2>
<h2>{{userWishList}}</h2>
</template>

<script lang="ts">
import { computed, defineComponent } from 'vue'
import { useWishlistStore } from '@/stores/useWishlistStore.ts'
export default defineComponent({
  name: 'WishList',

  setup() {
    // let userWishList;
    const wishListStore = useWishlistStore();
    const userWishList = computed(() => {
      console.log(wishListStore.getUserWishList())
      return wishListStore.getUserWishList();
    });

    return {
      userWishList
    };
  }
})
</script>

<style scoped lang="scss">

</style>
