<script setup lang="ts">
import { RouterLink, RouterView } from 'vue-router'

</script>

<template>
  <header>
    <nav>
      <ul>
        <li><router-link to="/">Home</router-link></li>
        <li><router-link to="/catalogue">Catalogue</router-link></li>
        <li><router-link to="/borrowhistory">Borrow History</router-link></li>
        <li><router-link to="/login">Log In</router-link></li>
        <li id="signup"><router-link to="/create-account">Create Account</router-link></li>
      </ul>
    </nav>
  </header>
  <RouterView />
</template>

<style lang="scss" >
@use "/src/assets/scss/colors.scss";

html, body, #app {
  margin: unset;
  padding: unset;
  width: 100%;
  height: 100vh;
  display: unset;
;
}
header {
  header{
    box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
  }
  nav {
    width: 100%;
    margin-top: 1rem;
    text-align: center;

    ul {
      display: flex;
      text-align: center;
      justify-content: center;
      align-items: center;

      li {
        font-weight: bolder;
        list-style: none;
        margin: 0 10px 10px ;
        font-size: 20px;
        padding: 20px;
        a{
          padding: 20px;
          color: colors.$text-color;
          font-weight: 600;
          border-radius: 2rem;
        }

      }
      #signup{

        a{
          color: colors.$color-primary;
          background-color: colors.$text-color;
          border-radius: 2rem;
        }
      }
    }
  }
}
</style>
