<template>
  <header id="header">
    <nav>
      <ul>
        <li id="welcome_username"  v-if="checkAuthenticated()">
         <p v-if="loggedInUser !=null"> Welcome back {{ loggedInUser.toUpperCase() }}</p>
        </li>
        <li>
          <router-link to="/" v-if="!checkAuthenticated()">Home</router-link>
        </li>
        <li id="log_in" v-if="!checkAuthenticated()">
          <router-link to="/login">Log In</router-link>
        </li>
        <li id="signup" v-if="!checkAuthenticated()">
          <router-link to="/create-account">Create Account</router-link>
        </li>
        <li v-if="checkAuthenticated()">
          <router-link to="/catalogue">Catalogue</router-link>
        </li>

        <li id="wishList" v-if="checkAuthenticated()">
          <router-link to="/wishlist">WishList</router-link>
        </li>
        <li v-if="checkAuthenticated()">
          <router-link to="/checkout"> Checkout</router-link>
        </li>
        <li id="add_book" v-if="checkAuthenticated()">
          <button @click="toggleFormModal">Add Book</button>
        </li>
        <li id="logout" v-if="checkAuthenticated()">
          <button @click="logOut">Log Out</button>
        </li>
      </ul>
    </nav>
  </header>
  <div id="add_new_book" v-if="isModalOpen">
    <form @submit.prevent="addNewBook">
      <div id="form_title">
        <p id="form_title_header">Add New Book</p>
         <p id="close_modal" @click="toggleFormModal">close</p>
      </div>
      <div>
        <label for="title">Title:</label>
        <input type="text" id="title" v-model="book.title"  required />
      </div>
      <div>
        <label for="author">Author:</label>
        <input type="text" id="author" v-model="book.author" required />
      </div>
      <div>
        <label for="isbn">Isbn:</label>
        <input type="number" id="isbn" v-model="book.isbn" required />
      </div>
      <div>
        <label for="publicationYear">Publication Year:</label>
        <input type="number" id="publicationYear" v-model="book.publicationYear" required />
      </div>
      <div>
        <label for="description">Description:</label>
        <textarea type="text" id="description" v-model="book.description"
                  required  placeholder="Enter Book Description"></textarea>"/>
      </div>
      <div>
        <label for="available">Available for Rent:</label>
         <select id="available" name="available" required  v-model="book.available">
          <option value="True" selected>True</option>
          <option value="False">False</option>
        </select>
      </div>
      <div>
        <label for="quantity">Quantity:</label>
        <input type="number" id="quantity" v-model="book.quantityForRent"  required />
      </div>
      <button type="submit" id="submit-form">Add Book</button>
    </form>
    <p id="book_added_confirmation" v-if="statusText">{{statusText}}</p>
  </div>
  <RouterView />
</template>
<script lang="ts">
import { useLogOutStore } from '@/stores/useLogOutStore.ts'
import router from '@/router'
import { useLogInStore } from '@/stores/useLogInStore.ts'
import { computed, onMounted, ref } from 'vue'
import { useAddBookCatalogueStore } from '@/stores/useBookCatalogueStore.ts'
import { username } from '../Utils/AppUtils.ts'

export default {
  name: 'MainHeader',

  setup() {
    const isModalOpen = ref(false)
    const catalogueStore = useAddBookCatalogueStore()
    const logoutStore = useLogOutStore()
    const logInStore = useLogInStore()
    const userProfile = ref('')
    const logOut = async () => {
      await logoutStore.LogOut()
      await router.push('/')
      router.go(0)
    }
    const getStatusText =computed(()=>catalogueStore.statusText);
    const toggleFormModal = () =>
      isModalOpen.value === true ? (isModalOpen.value = false) : (isModalOpen.value = true)

    const checkAuthenticated = () => {
      const checkAuth = logInStore.checkAuth()
      if (logInStore.statusCode === 200) {
        userProfile.value = JSON.stringify(localStorage.getItem('username'))
        router.push('/catalogue')
        router.go(0)
      }
      return checkAuth
    }
    onMounted(() => {
      if (!checkAuthenticated()) {
        router.push('/')
        return;
      }
    })
    return {
      book: catalogueStore.addBookCatalogue,
      logOut,
      checkAuthenticated,
      userProfile,
      addNewBook: catalogueStore.addBookToCatalogue,
      toggleFormModal,
      isModalOpen,
      statusText: getStatusText,
      loggedInUser: username,
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

header {
  box-shadow: 1px 4px 6px rgba(0, 0, 0, 0.1);
  width: 100%;
  background-color: white;
  z-index: 99999;
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
        font-size: 14px;
        padding: 20px;
        a {
          padding: 14px;
          color: colors.$text-color;
          font-weight: 600;
          border-radius: 2rem;
        }
      }
      #welcome_username{
        color: colors.$accent-color;
       p{
         border-radius: 2rem;
         padding: 10px;
         font-weight: bolder;
         background: #ecf0f1;
       }
      }
      #signup {
        padding: 15px;
        a {
          padding: 15px;
          color: colors.$white-color;
          background-color: colors.$text-color;
          border-radius: 0.5rem;
        }
      }

      #log_in {
        padding: 15px;
        a {
          border-radius: 0.5rem;
          padding: 15px;
          border: 0.1rem solid colors.$text-color;
          color: colors.$text-color;
        }
      }
      #logout {
        cursor: pointer;
        button {
          width: 100px;
          padding: 15px;
          cursor: pointer;
          border: none;
          outline: none;
          border-radius: 0.5rem;
          color: colors.$color-primary;
          font-weight: bold;
          background-color: colors.$error-color;
          &:hover {
            color: colors.$color-primary;
          }
        }
      }
      #add_book {
        button {
          width: 100px;
          padding: 15px;
          border: none;
          outline: none;
          border-radius: 0.5rem;
          color: colors.$color-primary;
          font-weight: bold;
          background-color: colors.$text-color;
          &:hover {
            color: colors.$color-primary;
          }
        }
      }
    }
  }

  #add_new_book {
    z-index: 999;
    position: fixed;
    //box-shadow: 1px 4px 6px rgba(0, 0, 0, 0.1);
    box-shadow: 0 0 50px #ccc;
    background-color: colors.$white-color;
    left: 200px;
    border-radius: 0.5rem;
    right: 200px;
    width: 40%;
    margin: 20px auto;
    #book_added_confirmation {
      margin: 0 auto;
      text-align: center;
      color: colors.$text-color;
      padding-bottom: 20px;
    }
    form {
      width: 60%;
      margin: 0 auto;
      padding-bottom: 10px;
      color: colors.$color-primary;
      div {
        color: colors.$color-primary;
        text-align: start;
        justify-items: start;
        input, select {
          width: 100%;
          padding: 10px;
          border: 1px solid colors.$text-color;
          border-radius: 0.5rem;
          margin: 5px 0;
          font-size: 16px;
        }
        textarea{
          border-radius: 0.5rem;
          margin: 5px 0 0;
          font-size: 16px;
          width: 100%;
          padding: 10px;
          height: 100px;
          resize: none;
          font: unset;
        }
        label {
          color: colors.$text-color;
          text-align: start;
          justify-items: start;
          align-items: start;
        }
      }
      button {
        cursor: pointer;
        width: 100%;
        margin: 10px 0 20px;
        padding: 15px ;
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
    }

    #form_title {
      display: flex;
      justify-content: space-evenly;
      align-items: center;
      #form_title_header {
        margin: 20px auto;
        text-align: center;
        font-weight: bolder;
        color: colors.$text-color;
        font-size: 30px;
      }
      #close_modal {
        cursor: pointer;
        background-color: colors.$color-primary;
        color: colors.$text-color;
        padding: 5px;
        border-radius: 0.5rem;
      }
    }
  }
}
</style>
