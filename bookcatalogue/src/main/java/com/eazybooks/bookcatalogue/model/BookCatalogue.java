package com.eazybooks.bookcatalogue.model;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;

@Entity
@Table(name="book_catalogue")
public class BookCatalogue {

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "book_catalogue_seq_generator")
  @SequenceGenerator(name = "book_catalogue_seq_generator", sequenceName = "book_catalogue_seq", allocationSize = 1, initialValue = 21)
  private Long id;
  private String title;
  private String author;
  private Long isbn;
  private int publicationYear;
  private String description;
  private boolean available;
  private int quantityForRent;

  public Long getId() {
    return id;
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

  public Long getIsbn() {
    return isbn;
  }

  public void setIsbn(Long isbn) {
    this.isbn = isbn;
  }

  public int getPublicationYear() {
    return publicationYear;
  }

  public void setPublicationYear(int publicationYear) {
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

  public void setId(Long id) {
    this.id = id;
  }

  @Override
  public String toString() {
    return "BookCatalogue{" +
        "Id=" + id +
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
