<template>
  <div id="confirm_checkout" v-if="isModalOpen">
<div>
  <p>{{confirmCheckoutStatusText}} </p>
  <button @click="toggleModal">Close</button>
</div>
  </div>
  <p v-if="checkoutInfo" id="checkoutInfo">{{ checkoutInfo }}</p>
  <CheckoutComponent :books="checkoutItemsList" @completeCheckout="confirmBookCheckout"/>
  <CheckoutHistory :checkout-history="checkoutHistoryList" :isSuccessful="returnStatus"
                   @checkout-again="checkOutAgain" @return-book="returnBook"/>
</template>
<script lang="ts">
import { computed, defineComponent, onBeforeMount, ref } from 'vue'
import type { BookCatalogue, CheckedOutHistory } from '@/model/model.ts'
import { useCheckoutItemStore } from '@/stores/useCheckoutItemStore.ts'
import CheckoutComponent from '@/views/CheckoutComponent.vue'
import CheckoutHistory from '@/views/CheckoutHistory.vue'
import { useCheckoutStore } from '@/stores/useCheckoutStore.ts'
import router from '@/router'
import { useCheckedOutHistory } from '@/stores/useCheckoutStore.ts'

export default defineComponent({
  name: 'check-out',
  components: { CheckoutComponent, CheckoutHistory },
  setup() {
    const isModalOpen = ref(false)
    const checkoutItemStore = useCheckoutItemStore()
    const checkoutHistory = useCheckedOutHistory()
    const checkoutStore = useCheckoutStore()
    const checkoutItemsList = ref<BookCatalogue[]>([])
    const checkoutHistoryList = ref<CheckedOutHistory[]>([])
    const checkoutInfo = ref(' ')
    const checkedOutHistoryInfo = ref('')

    const checkOutAgain = (bookIsbn: number) => {
      toggleModal();
      checkoutStore.checkBookOut(bookIsbn);
      setTimeout(() => {
        checkoutStore.$patch({ statusText: '' }); // Directly update the store
      }, 7000);
    }
    const returnBook = (bookIsbn: number) => {
      toggleModal();
      checkoutStore.returnBook(bookIsbn);
      setTimeout(() => {
        checkoutStore.$patch({ statusText: '' }); // Directly update the store
      }, 7000);
    }
    const confirmBookCheckout = (book: BookCatalogue) => {
      toggleModal();
      checkoutStore.checkBookOut(book.isbn);
      setTimeout(() => {
        checkoutStore.$patch({ statusText: '' }); // Directly update the store
      }, 7000);

    }
      const confirmCheckoutStatusText = computed(()=>{
      return checkoutStore.statusText;
    })
    const returnStatus = computed(()=>checkoutStore.statusCode === 200);
    const toggleModal = () =>{
      if ( isModalOpen.value === true ) {
        isModalOpen.value = false;
        router.go(0)
      }else{
        isModalOpen.value = true;
      }
    }

    onBeforeMount(async () => {
      await checkoutItemStore.getCatalogueItemsforUser()
      await checkoutHistory.getAllCheckoutHistoryForUser()
      if (checkoutItemStore.checkoutItems.length > 0) {
        checkoutItemsList.value = checkoutItemStore.checkoutItems

      } else {
        checkoutInfo.value = 'Checkout is empty'
      }
      if (checkoutHistory.checkedOutHistory.length > 0) {
        console.log(checkoutHistory.checkedOutHistory.at(0))
        checkoutHistoryList.value = checkoutHistory.checkedOutHistory
      } else {
        checkedOutHistoryInfo.value = 'No history found.'
      }
    })
    return {
      returnBook,
      checkoutInfo,
      checkoutItemsList,
      checkoutHistoryList,
      confirmBookCheckout,
      toggleModal,
      isModalOpen,
      confirmCheckoutStatusText,
      checkedOutHistoryInfo,
      checkOutAgain,
      returnStatus
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
#checkoutInfo{
  color: colors.$accent-color;
  margin: 50px auto;
  font-size: 36px;
  text-align: center;
}

#confirm_checkout {
  z-index: 999;
  position: fixed;
  //box-shadow: 1px 4px 6px rgba(0, 0, 0, 0.1);
  box-shadow: 0 0 50px #ccc;
  background-color: colors.$white-color;
  left: 200px;
  border-radius: 0.5rem;
  right: 200px;
  top: 250px;
  width: 40%;
  margin: 20px auto;
 padding: 40px;
  color: colors.$text-color;
  text-align: center;

    button {
      cursor: pointer;
      width: 150px;
      margin: 30px 0 20px;
      padding: 15px ;
      text-align: center;
      justify-items: center;
      background-color: colors.$text-color;
      border: none;
      color: colors.$white-color;
      font-weight: 400;
      border-radius: 0.5rem;
      font-size: 16px;
    }

}
</style>
