package ducttape.entities;

import javax.persistence.*;
import java.util.Date;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class WebOrder {

    @Id
    @GeneratedValue
    private Long id;

    @ManyToMany
    private List<Product> products;

    @OneToOne
    private Customer customer;

    private Date date;

    public WebOrder() {
    }

    public WebOrder(List<Product> products, Customer customer, Date date) {
        this.products = products;
        this.customer = customer;
        this.date = date;
    }

    public List<Product> getProducts() {
        return products;
    }

    public Customer getCustomer() {
        return customer;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }
}
