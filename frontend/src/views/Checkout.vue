<script lang="ts">
import { defineComponent } from 'vue'

export default defineComponent({
  name: 'BorrowHistory'
})
</script>

<template>
  <div class="wish-list-items">
    <div>
      <div class="wishes">
        <div class="wish-list-title"></div>
        <div class="remove-list"><button>Remove from cart</button></div>
        <div class="wish-list-checkout"><button>Checkout</button></div>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';
html, body {
  padding: 0;
  margin: 0;
}
</style>


