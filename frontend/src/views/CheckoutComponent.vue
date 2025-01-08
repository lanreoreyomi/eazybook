<template>

  <div class="checkout-container" v-for="(book, index) in books" :key="index">
    <div class="checkout_info">
      <div class="book_img">
        <img :src="`/src/assets/images/${index + 1}-min.jpg`" alt="lib_img" />
      </div>
      <div class="checkout_details">
        <ul>

          <li class="book_card_links" style="font-weight: bold"> Title: {{book.title}} </li>
          <li class="book_card_links"> Author: {{book.author}} </li>
          <li class="book_card_links"> Description: {{book.description}} </li>
          <li class="book_card_links">Book isbn: {{book.isbn}} </li>
          <li class="book_card_links">Book Publication year: {{book.publicationYear}} </li>
        </ul>
        <div class="checkout_button">
          <button class="checkout_remove-list" @click="removeBookFromCheckout(book)">Remove from cart</button>
          <button class="checkout-checkout">Complete checkout</button>
        </div>
      </div>
    </div>

  </div>
</template>
<script lang="ts">

import { computed, type PropType } from 'vue'
import type { BookCatalogue } from '@/model/model.ts'
import { useCheckoutStore } from '@/stores/useCheckoutStore.ts'
import router from '@/router'

export default {
  name: 'CheckoutComponent',

  props: {
    books: {
      type: Array as PropType<BookCatalogue[]>,  // Specify that 'book' should be a Book object
      required: true // Make the prop required
    }
  },

  setup() {
    const checkoutStore = useCheckoutStore();

   const removeBookFromCheckout = (book: BookCatalogue): void => {
      if (book !=null) {
        checkoutStore.removeBookFromCheckoutItem(book.isbn);
        router.go(0)
      }
    }
    const confirmationText  = computed(() => {
      return checkoutStore.statusText
    });

    return{
      removeBookFromCheckout,
      confirmationText
    }
  }
}
</script>
<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';

html,
body {
  padding: 0;
  margin: 0;
}

.checkout-container {
  width: 80%;
  justify-content: center;
  align-items: center;
  margin: 0 auto;
  .checkout_info {
    box-shadow: 0 0 50px #ccc;
    width: 70%;
    color: colors.$text-color;
    box-shadow: 0 0 50px #ccc;
    //display: grid;
    //grid-template-columns: 1fr 1fr;
    display: flex;
    flex-direction: row;
    background: colors.$accent-faint;
    align-items: center;
    margin: 50px auto;
    border-radius: 20px;
    padding: 30px;
    .checkout_details {
      width: 70%;
      padding: 20px 20px 0px 50px;

      ul {
        font-size: 16px;
        margin: unset;
        padding: unset;

        li {
          list-style: none;
          font-weight: normal;

        }
      }
    }

    .book_img {
      width: 30%;

      img {
        width: 100%;

      }
    }

    .checkout_button {
      display: grid;
      grid-template-columns: 1fr 1fr;
      padding-top: 10px;
      justify-content: center;

      button {
        border: none;
        background: none;
        text-align: center;
        justify-content: center;
        align-items: center;
        padding: unset;
      }

      .checkout_remove-list {
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

      .checkout-checkout {
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

  //.checkout_history {
  //  background: colors.$accent-faint;
  //  height: 50vh;
  //  border-radius: 10px;
  //
  //  h2 {
  //    text-align: center;
  //    padding: 20px;
  //    color: colors.$text-color;
  //  }
  //
  //  .checkout_history_info {
  //    color: colors.$text-color;
  //
  //    .checkout_history_table {
  //      text-align: center;
  //      padding: 80px;
  //
  //      thead {
  //        width: 100%;
  //
  //        tr {
  //          th,
  //          td {
  //            background-color: colors.$accent-faint;
  //            border-radius: 0.5rem;
  //            text-align: center;
  //            width: 10%;
  //            padding: 15px;
  //          }
  //        }
  //      }
  //
  //      tbody {
  //        width: 100%;
  //
  //        th,
  //        td {
  //          width: 10%;
  //          padding: 15px;
  //        }
  //
  //        td {
  //          button {
  //            font-size: 14px;
  //            padding: 15px;
  //            border-radius: 0.5rem;
  //            border: none;
  //            outline: none;
  //            cursor: pointer;
  //            margin: 10px;
  //            background: colors.$text-color;
  //            color: colors.$white-color;
  //          }
  //        }
  //      }
  //    }
  //  }
  //}
}
</style>
