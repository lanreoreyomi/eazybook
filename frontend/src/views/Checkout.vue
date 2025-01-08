<template>
  <CheckoutComponent :books="checkoutItemsList" />
  <CheckoutHistory />
</template>
<script lang="ts">
import { defineComponent, onBeforeMount, ref } from 'vue'
import type { BookCatalogue } from '@/model/model.ts'
import { useCheckoutStore } from '@/stores/useCheckoutStore.ts'
import CheckoutComponent from '@/views/CheckoutComponent.vue'

export default defineComponent({
  name: 'check-out',
  components: {CheckoutComponent },
  setup() {
    const checkoutStore = useCheckoutStore()
    const checkoutItemsList = ref<BookCatalogue[]>([])
    const checkoutInfo = ref(' ')

    onBeforeMount(async () => {
      await checkoutStore.getCatalogueItemsforUser()
      if (checkoutStore.checkoutItems.length > 0) {
        checkoutItemsList.value = checkoutStore.checkoutItems
      } else {
        checkoutInfo.value = 'Checkout is empty'
      }
    })
    return {
      checkoutInfo,
      checkoutItemsList
    }
  },
})
</script>

<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';

html,
body {
  padding: 0;
  margin: 0;
}
//.checkout-container {
//  width: 80%;
//  justify-content: center;
//  align-items: center;
//  margin: 0 auto;
//
//  .checkout_info {
//    box-shadow: 0 0 50px #ccc;
//    width: 50%;
//    color: colors.$text-color;
//    box-shadow: 0 0 50px #ccc;
//    //display: grid;
//    //grid-template-columns: 1fr 1fr;
//    display: flex;
//    flex-direction: row;
//    background: colors.$accent-faint;
//    align-items: center;
//    margin: 50px auto;
//    border-radius: 20px;
//
//    .checkout_details {
//      width: 70%;
//      padding: 20px 20px 0px 50px;
//      ul {
//        font-size: 16px;
//        margin: unset;
//        padding: unset;
//
//        li {
//          list-style: none;
//        }
//      }
//    }
//
//    .book_img {
//      width: 30%;
//      img {
//        width: 100%;
//        border-top-left-radius: 1rem;
//        border-bottom-left-radius: 1rem;
//      }
//    }
//
//    .checkout_button {
//      display: grid;
//      grid-template-columns: 1fr 1fr;
//      padding-top: 10px;
//      justify-content: center;
//      button {
//        border: none;
//        background: none;
//        text-align: center;
//        justify-content: center;
//        align-items: center;
//        padding: unset;
//      }
//
//      .checkout_remove-list {
//        font-size: 14px;
//        padding: 15px;
//        border-radius: 0.5rem;
//        border: none;
//        outline: none;
//        cursor: pointer;
//        margin: 10px;
//        background: colors.$error-color;
//        color: colors.$white-color;
//      }
//
//      .checkout-checkout {
//        font-size: 14px;
//        padding: 15px;
//        border-radius: 0.5rem;
//        border: none;
//        outline: none;
//        cursor: pointer;
//        margin: 10px;
//        background: colors.$text-color;
//        color: colors.$white-color;
//      }
//    }
//  }
//  .checkout_history {
//    background: colors.$accent-faint;
//    height: 50vh;
//    border-radius: 10px;
//    h2 {
//      text-align: center;
//      padding: 20px;
//      color: colors.$text-color;
//    }
//    .checkout_history_info {
//      color: colors.$text-color;
//      .checkout_history_table {
//        text-align: center;
//        padding: 80px;
//        thead {
//          width: 100%;
//          tr {
//            th,
//            td {
//              background-color: colors.$accent-faint;
//              border-radius: 0.5rem;
//              text-align: center;
//              width: 10%;
//              padding: 15px;
//            }
//          }
//        }
//        tbody {
//          width: 100%;
//          th,
//          td {
//            width: 10%;
//            padding: 15px;
//          }
//          td {
//            button {
//              font-size: 14px;
//              padding: 15px;
//              border-radius: 0.5rem;
//              border: none;
//              outline: none;
//              cursor: pointer;
//              margin: 10px;
//              background: colors.$text-color;
//              color: colors.$white-color;
//            }
//          }
//        }
//      }
//    }
//  }
//}
</style>
