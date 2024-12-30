<script setup lang="ts">

import { VueFinalModal } from 'vue-final-modal'
defineProps<{
  title?: string
}>()
</script>

<template>
  <VueFinalModal
    content-class="absolute bottom-0 w-full p-4 bg-red dark:bg-gray-900 space-y-2"
    swipe-to-close="down"
    content-transition="vfm-fade"
    overlay-transition="vfm-fade"
  style="background: #0b5345; height: 30vh; position: relative; left: 0; right: 0;
  bottom: 0; text-align: center; padding: 10px" >
    <h1 class="text-xl" >
      {{ title }}
    </h1>
    <div class="text-3xl" >Swipe down to close the modal</div>
    <slot />
  </VueFinalModal>
</template>

<style scoped lang="scss">

</style>
