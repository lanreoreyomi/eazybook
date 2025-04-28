<template>
   <div class="book_card" v-for="(book, index) in books" :key="index+1">
    <img :src="bookImages[index % bookImages.length]" alt="lib_img"  class="book_card-img" />
     <div class="book_card_info">
       <div class="book_card-body">
      <h5 class="book_card-title">{{ book.title }}</h5>
      <p class="book_card-text book_card-dec"> {{ book.description }}</p>
    </div>
    <div class="book_card_menu">
      <p class="book_card_links">Author: {{ book.author }}</p>
      <p class="book_card_links">{{ book.isbn }}</p>
      <p class="book_card_links">Year: {{ book.publicationYear }}</p>
      <p class="book_card_links">Available: {{ book.available }}</p>
      <p class="book_card_links">Quantity left: {{ book.quantityForRent }}</p>
    </div>
    <div class="book_card_button" >
      <p v-if="book.quantityForRent==0"><img src="https://img.icons8.com/?size=14&id=3156&format=png&color=000000" alt="icon"> Not available for checkout</p>
      <button class="book_card-link" id="save_book" @click="getCurrentBook(book)">Add to Wishlist</button>
      <button class="book_card-link" id="checkout_book" :class="{disabled_btn : book.quantityForRent===0}" @click="processCheckout(book)">Add to checkout</button>
    </div>
     </div>
  </div>
</template>
<script lang="ts" scoped>
import type { BookCatalogue } from '@/model/model.ts'
import { defineComponent, type PropType, } from 'vue'
import { bookImages } from '@/helper/ImageHelper.ts'


export default defineComponent({
  inheritAttrs: false,
  name: 'BookComponent',
  props: {
    books: {
      type: Array as PropType<BookCatalogue[]>,  // Specify that 'book' should be a Book object
      required: true // Make the prop required
    }
  },
  emits: ['getBookDetails', 'addToCheckout'],
  setup(props, {emit} ) {
    const getCurrentBook = (book: BookCatalogue) => {
      emit('getBookDetails', book);
    };
    const processCheckout = (book: BookCatalogue) => {
      emit('addToCheckout', book);
    };


    return {
      getCurrentBook,
      open,
      emit,
      processCheckout,
      bookImages


    }

  },
})
</script>
<style scoped lang="scss">
@use '/src/assets/scss/colors';

.book_card {
  border-radius: 1rem;
  align-content: center;
  justify-content: center;
  margin: 0 auto;
  width: 100%;
  font-size: 20px;
  .book_card-img {
    width: 100%;
  }
  .book_card_info {
    background-color: #f6f5f5;

    .book_card-body {
       border-radius: 0.5rem;
      .book_card-title {
        font-size: 20px;
        background: colors.$accent-faint;
        padding: 15px;
        border-radius: 0.5rem;


      }
    }

    .book_card-text {
      font-size: 16px;
      background: rgba(121, 224, 133, 0.1);
      padding: 20px;
      margin-top: 10px;
      border-radius: 0.5rem;

    }


    .book_card_menu {
      color: colors.$text-color;
      padding: 10px;
      font-size: 16px;
      list-style: none;
      margin: 10px;
      border-radius: 1rem;
      display: grid;
      grid-template-columns: 1fr 1fr;
      grid-gap: 4px;
      text-align: center;
      background: rgba(121, 224, 133, 0.03);

      .book_card_links {
        padding: 4px;
        background: white;
        border-radius: 0.5rem;

      }
    }

    .book_card_button {
      text-align: center;
      justify-content: center;

      button {
        font-size: 16px;
        margin: 0px 10px 20px auto;
        padding: 10px;
        border-radius: 0.5rem;
        border: none;
        outline: none;
        width: 180px;
        cursor: pointer;
        box-shadow: 2px 4px 6px rgba(0, 0, 0, 0.1);
      }

      #checkout_book {
        color: colors.$white-color;
        background-color: colors.$text-color;

        &:hover {
          background-color: colors.$white-color;
          color: colors.$text-color;
          border: 1px solid colors.$text-color;
        }
      }

      #save_book {
        background-color: colors.$white-color;
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

      .disabled_btn {
        opacity: 0.2;
        cursor: not-allowed;
      }
      p{
        font-size: 14px;
        color: colors.$error-color;
      }
    }
  }
}
</style>
