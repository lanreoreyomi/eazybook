<script setup lang="ts">
import MainHeader from '@/components/MainHeader.vue'
</script>
<template>
  <header>
    <MainHeader />
  </header>
</template>

<style lang="scss">
@use '/src/assets/scss/colors.scss';
html,
body,
#app {
  margin: 0;
  padding: 0;
  background-color: colors.$color-primary;
}
</style>
