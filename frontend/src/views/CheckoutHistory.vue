<template>
  <div class="checkout_history">
    <h2>Checkout History</h2>
    <div class="checkout_history_info">
      <table class="checkout_history_table">
        <thead>
        <tr style="text-align: right;">
          <th>Book Name</th>
          <th>Checked Out Date</th>
          <th>Returned date</th>
          <th>Action</th>
        </tr>
        </thead>
        <tbody>
        <tr>
          <td>Player 1</td>
          <td>Team A</td>
          <td>10</td>
          <td>
            <button>Checkout Again</button>
          </td>
        </tr>
        <tr>
          <td>Player 1</td>
          <td>Team A</td>
          <td>10</td>
          <td>
            <button>Checkout Again</button>
          </td>
        </tr>
        <tr>
          <td>Player 1</td>
          <td>Team A</td>
          <td>10</td>
          <td>
            <button>Checkout Again</button>
          </td>
        </tr>
        </tbody>
      </table>

    </div>
  </div>
</template>
<script lang="ts">
export default {
  name: 'CheckoutHistory'
}
</script>
<style scoped lang="scss">
@use '/src/assets/scss/colors.scss';

html,
body {
  padding: 0;
  margin: 0;
}

</style>