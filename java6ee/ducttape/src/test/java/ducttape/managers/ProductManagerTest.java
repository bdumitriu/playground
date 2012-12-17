package ducttape.managers;

import ducttape.entities.Product;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.asset.EmptyAsset;
import org.jboss.shrinkwrap.api.spec.JavaArchive;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ejb.EJB;
import java.util.List;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

@RunWith(Arquillian.class)
public class ProductManagerTest {

    @EJB
    private ProductManager productManager;

    @Deployment
    public static JavaArchive createDeployment() {
        return ShrinkWrap.create(JavaArchive.class, "test.jar")
                .addClass(ProductManager.class)
                .addPackage(Product.class.getPackage())
                .addClass(TestDataInserter.class)
                .addAsManifestResource("META-INF/test-persistence.xml", "persistence.xml")
                .addAsManifestResource(EmptyAsset.INSTANCE, "beans.xml");
    }

    @Test
    public void testIsDeployed() {
        Assert.assertNotNull(productManager);
    }

    @Test
    public void testListProducts() {
        final List<Product> products = productManager.listProducts();
        assertThat(products.size(), is(5));
    }

    @Test
    public void testProductOrder() {
        assertThat(productManager.listProducts(true, null).get(0).getId(), is(1l));
        assertThat(productManager.listProducts(false, null).get(0).getId(), is(5l));
    }

    @Test
    public void testFindById() {
        assertThat(productManager.findById(1l).getId(), is(1l));
    }
}
