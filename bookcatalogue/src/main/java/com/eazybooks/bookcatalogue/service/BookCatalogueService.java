package com.eazybooks.bookcatalogue.service;

import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.model.BookCatalogueImpl;
import com.eazybooks.bookcatalogue.repository.BookCatalogueRepository;
import jakarta.transaction.Transactional;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Transactional
public class BookCatalogueService implements BookCatalogueImpl {

  final BookCatalogueRepository bookCatalogueRepository;

  public BookCatalogueService(BookCatalogueRepository bookCatalogueRepository) {
    this.bookCatalogueRepository = bookCatalogueRepository;
  }

  @Override
  public List<BookCatalogue> getAllCatalogue() {
    return bookCatalogueRepository.findAll();
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
  public void deleteBookById(Long id) {
     bookCatalogueRepository.deleteById(id);
  }

  @Override
  public BookCatalogue getBookById(Long id) {
    return bookCatalogueRepository.findByBookId(id);
  }

  @Override
  public BookCatalogue updateBook(BookCatalogue book) {
    return bookCatalogueRepository.save(book);
  }

}
