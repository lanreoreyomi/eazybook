import axios from 'axios'
import { useAuthStore } from '@/stores/useAuthStore.ts'

const backEndUrl = 'http://ec2-34-227-14-199.compute-1.amazonaws.com';


const createAxiosInstance = () => {
  const axiosInstance = axios.create({
    baseURL: backEndUrl,
  });

  axiosInstance.interceptors.request.use(config => {
    const authStore = useAuthStore();
    if (authStore.token) {
      config.headers.Authorization = authStore.token; // Add "Bearer " prefix
     }
    return config;
  });

  return axiosInstance;
};

export const api = createAxiosInstance();

export const CREATEACCOUUNT = "/auth/create-account"
export const LOGIN =  "/auth/login"
export const LOGOUT = "/auth/logout"
export const BOOKCATALOGUE =  "/bookcatalogue/"
export const WISHLIST = "/wishlist"
export const CHECKOUTITEM = "/checkoutitems/"
export const CHECKBOOKOUT = "/checkout/"
export const CHECKOUTSTATS = "/stats/all"

export const getWishListForUser = (username: string): string => {
  return `${WISHLIST}/${username}/all`;
}

export const addBookToWishlistWithUsername = (username: string | null): string => {
  return `${WISHLIST}/${username}/add`
}
export const removeBookFromWishlistWithUsername = (username: string | null): string => {
  return `${WISHLIST}/${username}/remove`
}
export const addBookToCatalogueWithUsername = (username: string | null): string => {
  return `${BOOKCATALOGUE}${username}/addbook`
}
export const addBookToCatalogueItem = (username: string | null, isbn: number): string => {
  return `${CHECKOUTITEM}${username}/${isbn}/add`
}
export const removeBookFromCatalogueItem = (username: string | null, isbn: number): string => {
  return `${CHECKOUTITEM}${username}/${isbn}/remove`
}
export const getCatalogueItemsforUser = (username: string | null): string => {
  return `${CHECKOUTITEM}${username}/all`
}
export const processCheckout = (username: string | null, bookisbn: number): string => {
  return `${CHECKBOOKOUT}${username}/${bookisbn}`
}
export const returnCheckout = (username: string | null, bookisbn: number): string => {
  return `${CHECKBOOKOUT}${username}/${bookisbn}/return`
}
export const getAllCheckoutForUser = (username: string | null): string => {
  return `${CHECKBOOKOUT}${username}/all`
}
