<template>
  <div v-if="isCatalogueLoaded" id="catalogue_component">
    <div id="wishlist_control" v-if="confirmation">
      <p id="addedToWishlist">{{ confirmation }}</p>
      <p id="cancel_img">
        <img src="https://img.icons8.com/?size=30&id=7703&format=png&color=FFFFFF" alt="lib_img" @click="toggleConfirmation" />

      </p>
    </div>
    <div class="book_detail">
      <BookComponent :books="books" @getBookDetails="displayBook" />
    </div>
  </div>
</template>
<script lang="ts">
import { computed, defineComponent, onBeforeMount, ref } from 'vue'
import { useBookCatalogueStore } from '@/stores/useBookCatalogueStore.ts'
import type { BookCatalogue } from '@/model/model.ts'
import BookComponent from '@/components/BookComponent.vue'
export default defineComponent({
  name: 'CatalogueView',
  components: { BookComponent },
  setup() {
    const confirmation = ref('')
    const bookCatalogueStore = useBookCatalogueStore()
    const books = ref<BookCatalogue[]>([])
    const waitList: BookCatalogue[] = []

    const displayBook = (book: BookCatalogue): void => {addBookToWaitList(book)}
    const isCatalogueLoaded = computed(() => bookCatalogueStore.statusCode === 200)
    const addBookToWaitList = (book: BookCatalogue): void => {
       const existingBook = waitList.find((el: BookCatalogue) => el.isbn === book.isbn)
      if (existingBook) {
        confirmation.value = `${book.title} already added to wishlist`
      } else {
        waitList.push(book)
        confirmation.value = `Added ${book.title} to wishlist`
      }
    }
    const toggleConfirmation = function () {confirmation.value = ''}

    onBeforeMount(async () => {
       await bookCatalogueStore.getAllBookCatalogues()
       books.value = bookCatalogueStore.bookCatalogue
    })

    return {
      isCatalogueLoaded,
      books,
      displayBook,
      confirmation,
      toggleConfirmation,
      }
  },
})
</script>
<style lang="scss">
@use '/src/assets/scss/colors.scss';

#catalogue_component {
  padding: 10px;
  font-size: 16px;
  color: colors.$text-color;
  width: 100%;

  #wishlist_control {
    display: flex;
    flex-direction: row;
    position: sticky;
    top: 0;
    padding: 10px;
    font-size: 16px;
    color: colors.$white-color;
    text-align: center;
    width: 20%;
    background-color: colors.$text-color;
    border-radius: 0.5rem;
    align-content: center;
    box-shadow: 2px 4px 6px rgba(0, 0, 0, 0.1);
    justify-content: center;
    margin: 20px auto;

    #cancel_img {
      width: 10%;
      cursor: pointer;
       outline: none;
      border: none;
       img {
       padding-top: 5px;

      }
    }

    #addedToWishlist {
      width: 90%;
      padding-top: 10px;
    }
  }

  .book_detail {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr 1fr;
    grid-gap: 20px;
    padding: 30px;
    font-size: 16px;
    color: colors.$text-color;
    width: 100%;
  }
}
</style>
