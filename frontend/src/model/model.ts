export interface BookCatalogue {
  title: string
  author: string
  isbn: string
  publicationYear: string
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
