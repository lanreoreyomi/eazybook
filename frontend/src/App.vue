<template>
  <header>
  <MainHeader/>
  </header>
</template>
<script lang="ts">
import MainHeader from '@/components/MainHeader.vue'
import { defineComponent } from 'vue'
export default defineComponent({
  name: 'App',
  components: {MainHeader },

})
</script>
<style lang="scss">
@use '/src/assets/scss/colors.scss';
html,
body,
#app {
  margin: 0;
  padding: 0;
  background-color: colors.$white-color;
}
</style>
