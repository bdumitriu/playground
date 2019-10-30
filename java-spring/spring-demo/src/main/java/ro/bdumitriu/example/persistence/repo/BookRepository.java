package ro.bdumitriu.example.persistence.repo;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import ro.bdumitriu.example.persistence.model.Book;

public interface BookRepository extends CrudRepository<Book, Long> {

    List<Book> findByTitle(String title);
}
