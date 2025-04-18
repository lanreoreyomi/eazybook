export const CREATEACCOUUNT = '/auth/create-account'
export const LOGIN = '/auth/login'
export const LOGOUT = '/auth/logout'
export const BOOKCATALOGUE = '/bookcatalogue/'
export const WISHLIST = '/wishlist/'
export const CHECKOUTITEM = '/checkoutitems'
export const CHECKBOOKOUT = '/checkout'
export const CHECKOUTSTATS = '/stats/all'

export const getWishListForUser = (username: string| null): string => {
  return `${WISHLIST+username}/all `
}
export const addBookToWishlistWithUsername = (username: string | null): string => {
  return `${WISHLIST+username}/add`
}
export const removeBookFromWishlistWithUsername = (username: string | null): string => {
  return `${WISHLIST+username}/remove`
}
export const addBookToCatalogueWithUsername = (username: string | null): string => {
  return `${BOOKCATALOGUE}${username}/addbook`
}
export const addBookToCatalogueItem = (username: string | null, isbn: number): string => {
  return `${CHECKOUTITEM}/${username}/${isbn}/add`
}
export const removeBookFromCatalogueItem = (username: string | null, isbn: number): string => {
  return `${CHECKOUTITEM}/${username}/${isbn}/remove`
}
export const getCatalogueItemsforUser = (username: string | null): string => {
  return `${CHECKOUTITEM}/${username}/all`
}
export const processCheckout = (username: string | null, bookisbn: number): string => {
  return `${CHECKBOOKOUT}/${username}/${bookisbn}`
}
export const returnCheckout = (username: string | null, bookisbn: number): string => {
  return `${CHECKBOOKOUT}/${username}/${bookisbn}/return`
}
export const getAllCheckoutForUser = (username: string | null): string => {
  return `${CHECKBOOKOUT}/${username}/all`
}
