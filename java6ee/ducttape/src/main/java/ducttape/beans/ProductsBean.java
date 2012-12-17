package ducttape.beans;

import ducttape.entities.Product;
import ducttape.managers.ProductManager;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.inject.Named;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@RequestScoped
@Named
public class ProductsBean {

    @EJB
    private ProductManager productManager;

    private boolean asc;

    private String filter;

    public boolean isAsc() {
        return asc;
    }

    public void setAsc(boolean asc) {
        this.asc = asc;
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public String getFilter() {
        return filter;
    }

    public List<Product> getProducts() {
        return productManager.listProducts(asc, filter);
    }
}
