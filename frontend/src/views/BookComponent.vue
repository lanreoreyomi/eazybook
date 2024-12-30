<template>
   <div class="book_card" v-for="(book, index) in books" :key="index">
    <img class="book_card-img" src="../assets/images/library_bg.png" alt="lib_img" />
    <div class="book_card-body">
      <h5 class="book_card-title">{{ book.title }}</h5>
      <p class="book_card-text">Description: {{ book.description }}</p>
    </div>
    <ul class="book_card_menu">
      <li class="book_card_links">Author: {{ book.author }}</li>
      <li class="book_card_links">Year: {{ book.publicationYear }}</li>
      <li class="book_card_links">Available: {{ book.available }}</li>
      <li class="book_card_links">Quantity for rent: {{ book.quantityForRent }}</li>
    </ul>
    <div class="book_card_button" >
      <button class="book_card-link" id="save_book" @click="getCurrentBook(book)">Add to Wishlist</button>
      <button class="book_card-link" id="checkout_book">Checkout</button>
    </div>
  </div>
</template>
<script lang="ts" scoped>
import type { BookCatalogue } from '@/model/model.ts'
import { defineComponent, type PropType } from 'vue'

export default defineComponent({
  inheritAttrs: false,
  name: 'BookComponent',
    props: {
    books: {
      type: Array as PropType<BookCatalogue[]>,  // Specify that 'book' should be a Book object
      // type: Array as PropType<BookCatalogue[]>,  // Specify that 'book' should be a Book object
      required: true // Make the prop required
    }
  },
emits: ['getBookDetails'],
  setup(props, {emit} ) {
    const getCurrentBook = (book: BookCatalogue) => {
       emit('getBookDetails', book);
    };


    return {
      getCurrentBook,
      open,
      close,
      emit


    }
  },
})
</script>
<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';


  .book_card {
    width: 100%;
    font-size: 20px;
    box-shadow: 2px 4px 6px rgba(0, 0, 0, 0.1);
    border-radius: 0.5rem;
    background-color: #ecf0f1;
    .book_card-body {
      .book_card-title {
        padding: 10px;
        font-size: 20px;
        font-weight: bold;
      }
    }

    .book_card-text {
      padding: 10px;
      font-size: 16px;
    }
  }

  .book_card-img {
    padding: 0;
    width: 100%;
    border: 0.5rem;
  }

  .book_card_menu {
    font-size: 16px;
    list-style: none;
    padding: 10px;

    .book_card_links {
      font-weight: bold;
    }
  }

  .book_card_button {
    button {
      font-size: 16px;
      margin: 20px;
      padding: 10px;
      border-radius: 0.5rem;
      border: none;
      outline: none;
      width: 150px;
      cursor: pointer;
      box-shadow: 2px 4px 6px rgba(0, 0, 0, 0.1);
    }

    VButton {
      cursor: pointer;
    }

    #checkout_book {
      color: colors.$white-color;
      background-color: colors.$text-color;

      &:hover {
        background-color: colors.$white-color;
        color: colors.$text-color;
        border: 2px solid colors.$text-color;
      }
    }

    #save_book {
      background-color: colors.$white-color;
      border: 1px solid colors.$text-color;
      color: colors.$text-color;
    }

    #waitlist_confirmation {
      background-color: colors.$accent-color;
      position: sticky;
      right: 0;
      left: 0;
      text-align: center;
      align-items: center;
      justify-content: center;
      width: 300px;
      border: 3px solid #73AD21;

    }
  }

</style>
