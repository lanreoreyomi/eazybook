<template>
  <header>
  <MainHeader/>
  </header>
  <FooterComponent/>

</template>
<script lang="ts">
import MainHeader from '@/components/MainHeader.vue'
import FooterComponent from '@/components/Footer.vue'
import { defineComponent } from 'vue'
export default defineComponent({
  name: 'App',
  components: {MainHeader, FooterComponent },

})
</script>
<style lang="scss" scoped>
@use '/src/assets/scss/colors.scss';
html,
body,
#app {
  margin: 0;
  padding: 0;
  background-color: colors.$white-color;

}

</style>
