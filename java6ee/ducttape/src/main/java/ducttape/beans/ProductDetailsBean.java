package ducttape.beans;

import ducttape.entities.Product;
import ducttape.managers.ProductManager;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.inject.Named;

/**
 * @author Bogdan Dumitriu
 */
@RequestScoped
@Named
public class ProductDetailsBean {

    private Product product;

    private long pid;

    @EJB
    private ProductManager productManager;

    public void loadProduct() {
        product = productManager.findById(pid);
    }

    public Product getProduct() {
        return product;
    }

    public void setProduct(Product product) {
        this.product = product;
    }

    public long getPid() {
        return pid;
    }

    public void setPid(long pid) {
        this.pid = pid;
    }
}
