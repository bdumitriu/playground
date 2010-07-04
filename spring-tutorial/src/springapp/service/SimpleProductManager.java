package springapp.service;

import java.util.List;

import springapp.domain.Product;

public class SimpleProductManager implements ProductManager {

	private static final long serialVersionUID = 1L;

	private List<Product> products;

	@Override
	public List<Product> getProducts() {
		return products;
	}

	@Override
	public void increasePrice(int percentage) {
		if (products != null) {
			final double increaseFactor = 1.0 + (double) percentage / 100;
			for (Product product : products) {
				final Double currentPrice = product.getPrice();
				final Double increasedPrice = currentPrice * increaseFactor;
				product.setPrice(increasedPrice);
			}
		}
	}

	public void setProducts(List<Product> products) {
		this.products = products;
	}
}
