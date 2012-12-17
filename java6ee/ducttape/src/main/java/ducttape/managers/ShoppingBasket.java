package ducttape.managers;

import ducttape.entities.Customer;
import ducttape.entities.Product;
import ducttape.entities.WebOrder;

import javax.enterprise.context.Conversation;
import javax.enterprise.context.ConversationScoped;
import javax.enterprise.event.Event;
import javax.enterprise.inject.Any;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Named
@ConversationScoped
public class ShoppingBasket implements Serializable {

    @Inject
    private Conversation conversation;

    @Inject
    @Any // not need as it's the default, but kept for future reference
    private Event<WebOrder> webOrderEvent;

    private List<Product> productsInBasket = new ArrayList<>();

    private Customer customer = new Customer();

    public List<Product> getProductsInBasket() {
        return productsInBasket;
    }

    public Customer getCustomer() {
        return customer;
    }

    public void setCustomer(Customer customer) {
        this.customer = customer;
    }

    public void add(Product product) {
        if (conversation.isTransient()) {
            conversation.begin();
        }
        productsInBasket.add(product);
        System.out.println("Product added: " + product.getName());
    }

    public String checkout() {
        if (!conversation.isTransient()) {
            conversation.end();
        }

        System.out.println("Ordering for " + customer.getName());
        webOrderEvent.fire(new WebOrder(productsInBasket, customer, new Date()));
        
        return "index.xhtml?faces-redirect=true";
    }

    public BigDecimal getTotal() {
        BigDecimal total = new BigDecimal(0);
        for (Product product : productsInBasket) {
            total = total.add(product.getPrice());
        }
        return total;
    }
}
