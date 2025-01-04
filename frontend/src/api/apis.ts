export const CREATEACCOUUNT = '/auth/create-account'
export const LOGIN = '/auth/login'
export const LOGOUT = '/auth/logout'
export const GETALLBOOKCATALOGUE = '/bookcatalogue'
export const WISHLIST = '/wishlist/'

export const getWishListForUser = (username: string| null): string => {
  return `${WISHLIST+username}/all `
}
export const addBookToWishlistWithUsername = (username: string | null): string => {
  return `${WISHLIST+username}/add`
}
export const removeBookFromWishlistWithUsername = (username: string | null): string => {
  return `${WISHLIST+username}/remove`
}
