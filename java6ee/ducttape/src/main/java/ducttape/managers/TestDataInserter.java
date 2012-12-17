package ducttape.managers;

import ducttape.entities.Product;

import javax.annotation.PostConstruct;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.math.BigDecimal;

/**
 * @author Bogdan Dumitriu
 */
@Singleton
@Startup
public class TestDataInserter {

    @PersistenceContext
    private EntityManager entityManager;

    @PostConstruct
    public void insert() {
        for (int i = 1; i <= 5; i++) {
            entityManager.persist(new Product(i + " product", new BigDecimal(i * 10)));
        }
    }
}
