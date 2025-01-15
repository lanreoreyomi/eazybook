<template>

  <div class="stats" v-if="checkoutStats">
    <p> <span style='font-size:20px;'>&#127775;</span>
       {{checkoutStats.title}} is hot right now, Checkout out <span style='font-weight:bolder;'></span>{{checkoutStats?.totalCheckout}}  times</p>
  </div>
  <div v-if="isCatalogueLoaded" id="catalogue_component">
    <div id="wishlist_control" v-if="wishListStatusText">
      <p id="addedToWishlist">{{ wishListStatusText }}</p>
    </div>
    <div id="checkout_control" v-if="checkoutStatusText">
      <p id="addedToCheckout">{{ checkoutStatusText }}</p>
    </div>
    <div class="book_detail">
      <BookComponent :books="books" @getBookDetails="displayBook" @addToCheckout="bookCheckout" />
    </div>
  </div>
</template>

<script lang="ts">
import { computed, defineComponent, onBeforeMount, ref } from 'vue'
import { useBookCatalogueStore } from '@/stores/useBookCatalogueStore.ts'
import type { BookCatalogue } from '@/model/model.ts'
import BookComponent from '@/components/BookComponent.vue'
import { useWishlistStore } from '@/stores/useWishlistStore.ts'
import { useCheckoutItemStore } from '@/stores/useCheckoutItemStore.ts'
 import { useCheckoutStatsStore } from '@/stores/useCheckoutStatsStore.ts'
export default defineComponent({
  name: 'CatalogueView',
  components: { BookComponent },

  setup() {
    const confirmation = ref('')
    const bookCatalogueStore = useBookCatalogueStore()
    const checkoutStore = useCheckoutItemStore()
    const books = ref<BookCatalogue[]>([])
    const wishlistStore= useWishlistStore();
    const checkoutStatsStore = useCheckoutStatsStore()
    const bookCheckout = (book: BookCatalogue): void=>{
      if(book.quantityForRent > 0){
        checkoutStore.addBookToCheckoutItem(book.isbn);
      }
    }
    const checkoutStats = computed(() => checkoutStatsStore.stats)
    const displayBook = (book: BookCatalogue): void => {addBookToWishList(book)}
    const isCatalogueLoaded = computed(() => bookCatalogueStore.statusCode === 200)

    const addBookToWishList = (book: BookCatalogue): void => {
      wishlistStore.addBookToWishlist(book);
    }
    const wishListStatusText =  computed(()=> {
      const text = wishlistStore.statusText;
      // eslint-disable-next-line vue/no-async-in-computed-properties
      setTimeout(() => {
        wishlistStore.$patch({ statusText: '' }); // Directly update the store
      }, 7000);
      return text;
    });
    const checkoutStatusText =  computed(()=> {
      const text = checkoutStore.statusText;
      // eslint-disable-next-line vue/no-async-in-computed-properties
      setTimeout(() => {
        checkoutStore.$patch({ statusText: '' }); // Directly update the store
      }, 7000);
      return text;
    })

    onBeforeMount(async () => {
       await bookCatalogueStore.getAllBookCatalogues()
        await checkoutStatsStore.fetchCheckoutStats();
       books.value = bookCatalogueStore.bookCatalogue
    })

    return {
      isCatalogueLoaded,
      books,
      displayBook,
      confirmation,
      bookCheckout,
      wishListStatusText,
      checkoutStatusText,
      checkoutStats
      }
  },
})
</script>
<style lang="scss" scoped>
@use '/src/assets/scss/colors.scss';

.stats{
  padding: 8px;
  margin: 20px auto;
   width: 60%;
background-color: rgba(211, 211, 211, 0.36);
  border-radius: 0.5rem;
  p{
    text-align: center;
    color: colors.$text-color;

  }
}
#catalogue_component {
  font-size: 16px;
  color: colors.$text-color;
  width: 100%;

  #wishlist_control {
    position: sticky;
    top: 0;
    padding-bottom: 12px;
    padding-top: 8px;
    font-size: 16px;
    color: colors.$text-color;
    border: 1px solid colors.$text-color;
    text-align: center;
    width: 50%;
    background-color: colors.$white-color;
    border-radius: 0.5rem;
    align-content: center;
    box-shadow: 2px 4px 6px rgba(0, 0, 0, 0.1);
    justify-content: center;
    margin: 20px auto;
    #addedToWishlist {
       width: 90%;
      padding-top: 10px;
      font-weight: bold;


    }
  }

  #checkout_control {
    position: sticky;
    top: 0;
    padding-bottom: 12px;
    padding-top: 12px;
    font-size: 16px;
    color: colors.$white-color;
    text-align: center;
    width: 50%;
    background-color: colors.$text-color;
    border-radius: 0.5rem;
    align-content: center;
    box-shadow: 2px 4px 6px rgba(0, 0, 0, 0.1);
    justify-content: center;
    margin: 20px auto;
    #addedToWishlist {
      width: 90%;
      padding-top: 10px;
      font-weight: bold;


    }
  }

  .book_detail {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr 1fr;
    grid-gap: 20px;
    padding: 10px;
    font-size: 16px;
    color: colors.$text-color;
    width: 100%;
  }
}
</style>
