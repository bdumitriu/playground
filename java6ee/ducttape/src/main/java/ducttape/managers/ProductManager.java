package ducttape.managers;

import ducttape.entities.Product;

import javax.ejb.Stateless;
import javax.enterprise.inject.Produces;
import javax.inject.Named;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
public class ProductManager {

    @PersistenceContext
    private EntityManager entityManager;

    @Produces
    @Named
    public List<Product> listProducts() {
        return listProducts(true, null);
    }

    public List<Product> listProducts(boolean asc, String filter) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Product> query = criteriaBuilder.createQuery(Product.class);
        final Root<Product> root = query.from(Product.class);
        query.select(root);
        if (asc) {
            query.orderBy(criteriaBuilder.asc(root.get("price")));
        } else {
            query.orderBy(criteriaBuilder.desc(root.get("price")));
        }
        if (filter != null && filter.length() > 0) {
            query.where(criteriaBuilder.like(root.<String>get("name"), filter + "%"));
        }
        return entityManager.createQuery(query).getResultList();
    }

    public Product findById(long pid) {
        return entityManager.find(Product.class, pid);
    }
}
