package com.eazybooks.bookcatalogue.controller;


import com.eazybooks.bookcatalogue.model.BookCatalogue;
import com.eazybooks.bookcatalogue.service.BookCatalogueService;
import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/bookcatalogue")
public class BookCatalogueController {

  private final BookCatalogueService bookCatalogueService;


  public BookCatalogueController(BookCatalogueService bookCatalogueService) {
    this.bookCatalogueService = bookCatalogueService;
  }

  @GetMapping("/allbooks")
  public ResponseEntity<List<BookCatalogue>> getAllBookCatalogues() {

    try{

      final List<BookCatalogue> allCatalogue = bookCatalogueService.getAllCatalogue();
      return ResponseEntity.status(HttpStatus.OK).body(allCatalogue);
    } catch (Exception e) {
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
     }

  }
}
