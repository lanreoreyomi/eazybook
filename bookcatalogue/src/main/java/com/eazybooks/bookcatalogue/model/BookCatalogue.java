package com.eazybooks.bookcatalogue.model;

import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import java.awt.print.Book;
import java.util.List;

@Entity
@Table(name="book_catalogue")
public class BookCatalogue {

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE)
  @Column(nullable = false)
  private Long id;
  private String title;

  private String author;
  private String isbn;
  private String publicationYear;
  private String description;
  private boolean available;
  private int quantityForRent;

  //I need to build the user Entity and so proper mapping. and then update the service and controller classes.

  public void setId(Long id) {
    this.id = id;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public String getAuthor() {
    return author;
  }

  public void setAuthor(String author) {
    this.author = author;
  }

  public String getIsbn() {
    return isbn;
  }

  public void setIsbn(String isbn) {
    this.isbn = isbn;
  }

  public String getPublicationYear() {
    return publicationYear;
  }

  public void setPublicationYear(String publicationYear) {
    this.publicationYear = publicationYear;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public boolean isAvailable() {
    return available;
  }

  public void setAvailable(boolean available) {
    this.available = available;
  }

  public int getQuantityForRent() {
    return quantityForRent;
  }

  public void setQuantityForRent(int quantityForRent) {
    this.quantityForRent = quantityForRent;
  }

  @Override
  public String toString() {
    return "BookCatalogue{" +
        "id=" + id +
        ", title='" + title + '\'' +
        ", author='" + author + '\'' +
        ", isbn='" + isbn + '\'' +
        ", publicationYear=" + publicationYear +
        ", description='" + description + '\'' +
        ", isAvailable=" + available +
        ", quantityForRent=" + quantityForRent +
        '}';
  }

  public BookCatalogue() {
  }
}
