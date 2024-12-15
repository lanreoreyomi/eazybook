<template>

<div id="create-account-form">

  <form @submit.prevent="createUser">
    <div>
      <h2>Create Account</h2>
    </div>
    <div>
      <label for="username">Username:</label>
      <input type="text" id="username" v-model="user.username" required>
    </div>
    <div>
      <label for="password">Password:</label>
      <input type="password" id="password" v-model="user.password" required>
    </div>
    <div>
      <label for="firstname">First Name:</label>
      <input type="text" id="firstname" v-model="user.firstname" required>
    </div>
    <div>
      <label for="lastname">Last Name:</label>
      <input type="text" id="lastname" v-model="user.lastname" required>
    </div>
    <div>
      <label for="email">Email:</label>
      <input type="email" id="email" v-model="user.email" required>
    </div>
    <button type="submit" id="submit-form">Create Account</button>

    <div v-if="isLoading">Creating user...</div>
    <div v-if="error">Error: {{ error.message }}</div>
  </form>

</div>
</template>
<script>
import { useUserStore } from '@/stores/useUserStore.js'

export default {
  setup() {
    const userStore = useUserStore();
    return { user: userStore.user,
      isLoading: userStore.loading,
      error: userStore.error,
      createUser: userStore.createUser };
  }
};
</script>

<style lang="scss">
@use "src/assets/scss/colors.scss";

html, body, #app {
  margin: unset;
  padding: unset;
  width: 100%;
  height: 100vh;
  display: unset;
}
#create-account-form{
  height: 100vh;
  padding: 80px;
    justify-items: center;
  margin: 0 auto;
  div{
    h2{
      font-weight: bolder;
      color: colors.$accent-color;
      font-size: 50px;
      padding-bottom: 40px;
    }
  }

  form{
    color: colors.$color-primary;
     div{
       color: colors.$color-primary;
       text-align: start;
        justify-items: start;
        input {
          width: 100%;
          padding: 20px;
          background-color: colors.$color-primary;
          border: 1px solid colors.$text-color;
          border-radius: 2rem;
          margin: 10px 0;
          font-size: 20px;
      }
       label{
         color: colors.$text-color;
         text-align: start;
         justify-items: start;
         align-items: start;

       }
    }
    button{
      width: 100%;
      margin: 20px 0;
      padding: 20px;
      text-align: center;
      justify-items: center;
       background-color: colors.$text-color;
       border: none;
       color: colors.$color-primary;
      font-weight: 600;
      border-radius: 2rem;
      font-size: 20px;
    }
  }
}

</style>
