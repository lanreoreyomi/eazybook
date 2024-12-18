<script setup lang="ts"></script>

<template>
  <h1>This is home</h1>
</template>

<style lang="scss" scoped>
@use '/src/assets/scss/colors.scss';
html,
body,
#app {
  margin: 0;
  padding: 0;
}
h1 {
  color: colors.$text-color;
  font-weight: bolder;
  font-size: 56px;
}
</style>
