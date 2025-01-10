
export interface BookCatalogue {
  title: string
  author: string
  isbn: number
  publicationYear: number
  description: string
  available: boolean
  quantityForRent: number

}

export interface WishList {
  localDate: string,
  username: string,
  bookTitle: string,
  isbn: number

}
export interface CheckedOutHistory {
  nameOfBook: string;
  bookIsbn: number;
  checkoutDate: string;
  returnStatus: boolean;
  expectedReturnDate: string;
}

export interface Checkout {
  id: string,
  username: string,
}
