<template>
  <p v-if="checkoutInfo" id="checkoutInfo">{{ checkoutInfo }}</p>
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
  components: { CheckoutComponent },
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
      checkoutItemsList,
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
</style>
