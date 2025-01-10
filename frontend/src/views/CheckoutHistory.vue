<template>
  <div class="checkout_history" v-if="checkoutHistory.length>0">
    <h2>Checkout History</h2>
    <div class="checkout_history_info">
      <table class="checkout_history_table">
        <thead>
        <tr>
          <th>Book Name</th>
          <th>Book Isbn</th>
          <th>Checked Out Date</th>
          <th>Expected Returned date</th>
          <th>Return Status</th>
           <th>Return</th>
        </tr>
        </thead>
        <tbody>
        <tr v-for="(history,index) in checkoutHistory" :key='index'>
          <td>{{history.nameOfBook}}</td>
          <td>{{history.bookIsbn}}</td>
          <td>{{history.checkoutDate}}</td>
          <td>{{history.expectedReturnDate}}</td>
          <td v-if="history.returnStatus" style="font-weight: bolder">Returned</td>
          <td v-else>Not Returned</td>
          <td class="returnbook">
            <button class="returnbook" @click="returnBook(history.bookIsbn)">Return Book</button>
           </td>
        </tr>
        </tbody>
      </table>
    </div>
  </div>
</template>
<script lang="ts">
import type { PropType } from 'vue'
import type { CheckedOutHistory } from '@/model/model.ts'

export default {
  name: 'CheckoutHistory',
  props: {
    checkoutHistory: {
      type: Array as PropType<CheckedOutHistory[]>,  // Specify that 'book' should be a Book object
      required: true // Make the prop required
    },
    isSuccessful:{
      type: Boolean,
      default: false
    }
  },
 emits: ['returnBook', 'checkoutAgain'],
  setup(props, { emit }) {
    const returnBook = (bookIsbn: number) => {
      emit('returnBook', bookIsbn)
    }
   const checkOutAgain = (bookIsbn: number) => {
      emit('checkoutAgain', bookIsbn)
    }
    return {
      returnBook,
      checkOutAgain,

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
.checkout_history {
  background: colors.$accent-faint;

  border-radius: 10px;

  h2 {
    text-align: center;
    padding: 20px;
    color: colors.$text-color;
  }

  .checkout_history_info {
    color: colors.$text-color;
    .checkout_history_table {
      text-align: center;
      padding: 80px;

      thead {
        width: 100%;
        tr {
          th,
          td {
            font-weight: bold;
            background-color: colors.$accent-faint;
            border-radius: 0.5rem;
            text-align: center;
            width: 10%;
            padding: 15px;
          }
        }
      }

      tbody {
        width: 100%;
        th,
        td {
          width: 10%;
          padding: 15px;
        }
        td {
          button {
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
          .returnbook {
            background-color: colors.$accent-faint;
            color: colors.$text-color;
            border: 1px solid colors.$text-color;
          }

        }

      }
    }
  }
}

</style>
