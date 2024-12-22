<template>
  <header v-if="isSuccessful">
    <AuthenticatedHeader />
  </header>
  <header v-else>
    <MainHeader />
  </header>
</template>
<script lang="ts">
import { computed, defineComponent } from 'vue'
import { useLogInStore } from '@/stores/useLogInStore.ts'
import MainHeader from '@/components/MainHeader.vue'
import AuthenticatedHeader from '@/components/AuthenticatedHeader.vue'
export default defineComponent({
  name: 'App',
  components: { AuthenticatedHeader, MainHeader },
  setup: function () {
    const logInStore = useLogInStore()
    const isSuccessful = computed(() => logInStore.isLoggedIn)
    return {
      logInStore,
      isSuccessful,
    }
  },
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
