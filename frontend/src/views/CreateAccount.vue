<template>
  <div id="create-account-form">
    <form @submit.prevent="createUser">
      <div>
        <h2>Create Account</h2>
      </div>
      <div>
        <label for="username">Username:</label>
        <input type="text" id="username" v-model="user.username" required />
      </div>
      <div>
        <label for="password">Password:</label>
        <input type="password" id="password" v-model="user.password" required />
      </div>
      <div>
        <label for="firstname">First Name:</label>
        <input type="text" id="firstname" v-model="user.firstname" required />
      </div>
      <div>
        <label for="lastname">Last Name:</label>
        <input type="text" id="lastname" v-model="user.lastname" required />
      </div>
      <div>
        <label for="email">Email:</label>
        <input type="email" id="email" v-model="user.email" required />
      </div>
      <button type="submit" id="submit-form">Create Account</button>

      <div v-if="isSuccessful" id="user_created">{{ statusText }}</div>
      <div v-else-if="!isSuccessful" id="user_not_created">{{ statusText }}</div>
    </form>
  </div>
</template>
<script lang="ts">
import { useUserStore } from '@/stores/useUserStore.ts'
import { computed } from 'vue'
import router from '@/router'

export default {
  setup() {
    const userStore = useUserStore()
    const isSuccessful = computed(
      () =>{
        if (userStore.statusCode === 201) {
        router.push("/login");
          return true
        }else {
          return false
        }

      }
    )
    const statusText = computed(() => userStore.statusText)
    return {
      user: userStore.user,
      createUser: userStore.createUser,
      isSuccessful,
      statusText,
    }
  },
}
</script>

<style lang="scss" scoped>
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
#create-account-form {
   padding: 20px;
  justify-items: center;
  margin: 0 auto;
  div {
    h2 {
      text-align: center;
      font-weight: bolder;
      color: colors.$accent-color;
      font-size: 40px;
      padding-bottom: 20px;
    }
  }

  form {
    color: colors.$color-primary;
    div {
      color: colors.$color-primary;
      text-align: start;
      justify-items: start;
      input {
        width: 100%;
        padding: 15px;
         border: 1px solid colors.$text-color;
        border-radius: 0.5rem;
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
      padding: 15px;
      text-align: center;
      justify-items: center;
      background-color: colors.$text-color;
      border: none;
      color: colors.$white-color;
      font-weight: 400;
      border-radius: 0.5rem;
      font-size: 16px;
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
