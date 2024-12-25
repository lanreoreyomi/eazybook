<script lang="ts">
import { defineComponent, computed, onBeforeMount } from 'vue'
import { useBookCatalogueStore } from '@/stores/BookCatalogue.ts'


export default defineComponent({
  name: 'CatalogueView',
  setup() {
    const bookCatalogueStore = useBookCatalogueStore();

    // Fetch the catalog when the component is created
    bookCatalogueStore.getAllBookCatalogues();

    const isCatalogueLoaded = computed(() =>

      bookCatalogueStore.statusCode === 200);
    const catalogueData = computed(() => bookCatalogueStore.bookCatalogue);
    const dd = computed(() => {
      return bookCatalogueStore.statusCode;
    });

    return {
      isCatalogueLoaded,
      catalogueData,
      dd,
    };

    onBeforeMount(async () => {
      await logoutStore.checkAuth(); // Assuming you have this method in your store
    });
  },

});
</script>

<template>
<h2 v-if="isCatalogueLoaded">{{catalogueData}}</h2>
</template>

<style scoped lang="scss">

h2{
  color:red;
}

</style>
