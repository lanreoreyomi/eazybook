
<template>

<div id="logInForm">
  <form @submit.prevent="logIn">
    <div>
      <h2>Log In</h2>
    </div>
    <div>
      <label for="username">Username:</label>
      <input type="text" id="username" v-model="user.username" required />
    </div>
    <div>
      <label for="password">Password:</label>
      <input type="password" id="password" v-model="user.password" required />
    </div>
    <button type="submit" id="submit-form">Log In</button>

    <div v-if="isSuccessful" id="user_created">{{statusText}}</div>
    <div v-else-if="!isSuccessful" id="user_not_created">{{statusText}}</div>
  </form>
</div>
</template>
<script lang="ts">
 import { useLogInStore } from '@/stores/useLogInStore.js'
import { computed } from 'vue'

export default {
  setup() {
    const logInStore = useLogInStore()
    const isSuccessful = computed(() => logInStore.statusCode == 200);
    const statusText = computed(() => logInStore.statusText)

    return {
      user: logInStore.userLogIn,
      isLoading: logInStore.loading,
       logIn: logInStore.LogIn,
      isSuccessful,
      statusText
     }
  },
}
</script>

<style lang="scss">
@use '/src/assets/scss/colors.scss';

html,
body,
#app {
  margin: unset;
  padding: unset;
  width: 100%;
  height: 100vh;
  display: unset;
}
#logInForm {
  margin: 0 auto;
  width: 20%;
  form {
    color: colors.$color-primary;
    div {
      color: colors.$color-primary;
      text-align: start;
      justify-items: start;

      h2{
        padding: 20px;
        color: colors.$text-color;
         font-size: 40px;
        font-weight: bold;
      }
      input {
        width: 100%;
        padding: 10px;
        border: 1px solid colors.$text-color;
        border-radius: 1rem;
        margin: 10px 0;
        font-size: 20px;
      }
      label {
        color: colors.$text-color;
        text-align: start;
        justify-items: start;
        align-items: start;
      }
    }
    button {
      width: 100%;
      margin: 20px 0;
      padding: 20px;
      text-align: center;
      justify-items: center;
      background-color: colors.$text-color;
      border: none;
      color: colors.$white-color;
      font-weight: 600;
      border-radius: 0.5rem;
      font-size: 20px;
    }
    #user_created {
      color: colors.$text-color;
      font-weight: bolder;
    }
    #user_not_created {
      color: colors.$error-color;
      font-weight: bolder;
    }
  }
}

</style>
