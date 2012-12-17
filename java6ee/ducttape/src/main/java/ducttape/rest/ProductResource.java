package ducttape.rest;

import ducttape.entities.Product;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Path("products")
@Stateless
public class ProductResource {

    @Inject
    private List<Product> products;

    @GET
    public List<Product> get() {
        return products;
    }
}
