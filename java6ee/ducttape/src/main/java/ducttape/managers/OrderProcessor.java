package ducttape.managers;

import ducttape.entities.WebOrder;

import javax.ejb.Stateless;
import javax.enterprise.event.Observes;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
public class OrderProcessor {

    @PersistenceContext
    private EntityManager entityManager;

    public void processOrder(@Observes WebOrder webOrder) {
        entityManager.persist(webOrder.getCustomer());
        entityManager.persist(webOrder);
    }
}
