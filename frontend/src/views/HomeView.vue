
<template>
  <div id="landing_container">
    <div id="landing_text">
      <h1>EazyBooks</h1>
      <h1 id="supporting_h1">Read the smart way</h1>
      <p>Eazybook is an online library that provides convenient access to a vast collection of books, audiobooks, and other digital content. Explore our extensive catalog, borrow titles with ease, and enjoy reading anytime, anywhere.</p>
      <div id="store_metrics">
        <div>30,000 Books</div>
        <div>1200 Daily Checkouts</div>
        <div>300 Daily additions</div>
      </div>
    </div>
    <div id="landing_img">
      <img :src=bg_img alt="landing_page">
    </div>

  </div>
</template>
<script setup lang="ts">
import bg_img from '/src/assets/images/bg_img.jpg'
import { onMounted } from 'vue'
import { useAuthStore } from '@/stores/useAuthStore.ts'
import router from '@/router'

onMounted( async () => {
  const authStore = useAuthStore();
   if (!authStore.username) {
    await router.push('/')
    return;
  }

})

</script>

<style lang="scss" scoped>
@use '/src/assets/scss/colors.scss';
html,
body,
#app {
  margin: 0;
  padding: 0;
}
h1 {

  font-weight: bolder;
  font-size: 56px;
}

#landing_container{
  margin: 10px auto;
  align-items: center;
  justify-content: center;
  color: colors.$text-color;
  display: flex;
  flex-direction: row;

  #landing_img{
    width: 70%;
    img{
      //box-shadow: 0px 0px 15px 5px rgba(11, 83, 69, 0.5); /* Blue shadow with blur */

      //border-radius: 8%;
      width: 100%;
    }
  }
  #landing_text{
    padding: 40px 0;
    margin: 40px 40px;
    width: 35%;
    color: colors.$accent-color;
    h1{
      font-size: 56px;
      font-weight: 800;
      color: colors.$text-color;
    }
    #supporting_h1{
      color: colors.$accent-color;
    }
    h3{
      font-size: 36px;
    }

    #landing_text_buttons{
      margin-top: 40px ;
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
          padding: 20px;
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
            border-radius: 1.5rem;
          }
        }
        #log_in {

          a {
            border-radius: 1.5rem;
            padding: 15px;
            border: 0.1rem solid colors.$text-color;
            color: colors.$text-color;
          }
        }

      }
    }
  }
  #store_metrics{
    display: flex;
    width: 100%;
    margin-top: 40px;
    text-align: center;
    justify-content: center;
    background-color: #ecf0f1;
    border-radius: 0.5rem;

    div{
      border: 1px solid colors.$accent-color;
      border-radius: 0.5rem;
      margin: 20px;
      color: colors.$text-color;
      padding: 10px;
    }
  }
}
</style>
