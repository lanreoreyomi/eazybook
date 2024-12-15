<script setup>
 import MainHeader from '@/components/main-header.vue'
  </script>
<template>
  <header>
    <main-header/>
  </header>
 </template>

<style lang="scss" >
@use "src/assets/scss/colors.scss";
html, body, #app {
  margin: 0;
  padding: 0;
  background-color: colors.$color-primary;
}


</style>
