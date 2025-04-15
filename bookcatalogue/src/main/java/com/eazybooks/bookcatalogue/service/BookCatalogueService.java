package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.interfaces.IBookCatalogue;
import com.eazybooks.bookcatalogue.repository.BookCatalogueRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class BookCatalogueService implements IBookCatalogue {

  final BookCatalogueRepository bookCatalogueRepository;

  public BookCatalogueService(BookCatalogueRepository bookCatalogueRepository) {
    this.bookCatalogueRepository = bookCatalogueRepository;
  }

  @Override
  public List<BookCatalogue> getAllCatalogue(Pageable pageable) {
    final Page<BookCatalogue> all = bookCatalogueRepository.findAll(pageable);
    return all.stream().collect(Collectors.toList());
  }

  @Override
  public List<BookCatalogue> getBookByAuthor(String author) {
    return bookCatalogueRepository.findByAuthor(author);
  }

  @Override
  public BookCatalogue addBookToCatalogue(BookCatalogue book) {
    return bookCatalogueRepository.save(book);
  }

  @Override
  public BookCatalogue getBookById(Long id) {
    return bookCatalogueRepository.findById(id).orElse(null);
  }

  @Override
  public BookCatalogue getBookByIsbn(Long isbn) {
    return bookCatalogueRepository.findByBookByIsbn(isbn);
  }

  @Override
  public BookCatalogue updateBook(BookCatalogue book) {
    return bookCatalogueRepository.save(book);
  }

}
