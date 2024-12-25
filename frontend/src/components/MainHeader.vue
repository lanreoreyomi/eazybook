<script lang="ts">
import { useLogOutStore } from '@/stores/useLogOutStore.ts';
import { computed, onBeforeMount } from 'vue'
import router from '@/router';

export default {
  name: 'MainHeader',
  setup() {
    const logoutStore = useLogOutStore();
    const isLoggedOut = computed(() => !logoutStore.isLoggedIn); // Check login status directly

    const logOut = async () => {
      await logoutStore.LogOut();
      await router.push('/');
    };

    // Check login status when the component mounts
    onBeforeMount(async () => {
      logoutStore.checkAuth(); // Assuming you have this method in your store
    });

    return {
      logOut,
      isLoggedOut,
    };
  }
};
</script>

<template>
  <header>
    <nav>
      <ul>
        <li><router-link to="/" v-if="isLoggedOut">Home</router-link></li>
         <li id="log_in" v-if="isLoggedOut"><router-link to="/login">Log In</router-link></li>
        <li id="signup" v-if="isLoggedOut"><router-link to="/create-account">Create Account</router-link></li>
        <li  v-if="!isLoggedOut"><router-link to="/catalogue">Catalogue</router-link></li>
        <li v-if="!isLoggedOut"><router-link to="/borrowhistory">Borrow History</router-link></li>
        <li id="profile"  v-if="!isLoggedOut"><router-link to="/profile">Profile</router-link></li>
        <li id="wishList"  v-if="!isLoggedOut"><router-link to="/wishlist">WishList</router-link></li>
        <li id="logout"  v-if="!isLoggedOut"><button @click="logOut">Log Out</button></li>
      </ul>
    </nav>
  </header>
  <RouterView />
</template>

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
header {
  header {
    box-shadow: 1px 4px 6px rgba(0, 0, 0, 0.1);
  }
  nav {
    width: 100%;
    margin-top: 1rem;
    text-align: center;

    ul {
      padding: 0;
      margin: 0;
      display: flex;
      text-align: center;
      justify-content: center;
      align-items: center;
      li {
        list-style: none;
        font-size: 16px;
        padding: 30px;
        a {
          padding: 20px;
          color: colors.$text-color;
          font-weight: 600;
          border-radius: 2rem;
        }
      }
      #signup {
        a {
          padding: 15px;
          color: colors.$white-color;
          background-color: colors.$text-color;
          border-radius: 0.5rem;
        }
      }
      #log_in {

        a {
          border-radius: 0.5rem;
          padding: 15px;
          border: 0.1rem solid colors.$text-color;
          color: colors.$text-color;
        }
      }

    }
  }
}
</style>
