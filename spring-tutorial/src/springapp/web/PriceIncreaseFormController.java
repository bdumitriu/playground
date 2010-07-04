package springapp.web;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.SimpleFormController;
import org.springframework.web.servlet.view.RedirectView;

import springapp.service.PriceIncrease;
import springapp.service.ProductManager;

public class PriceIncreaseFormController extends SimpleFormController {

	protected final Log logger = LogFactory.getLog(getClass());

	private ProductManager productManager;

	@Override
	protected ModelAndView onSubmit(Object command) throws ServletException {
		int increasePercentage = ((PriceIncrease) command).getPercentage();
		logger.info("Increasing prices by " + increasePercentage + "%.");

		productManager.increasePrice(increasePercentage);
		logger.info("Returning from PriceIncreaseForm view to " + getSuccessView());

		return new ModelAndView(new RedirectView(getSuccessView()));
	}

	@Override
	protected Object formBackingObject(HttpServletRequest request) throws ServletException {
		PriceIncrease priceIncrease = new PriceIncrease();
		priceIncrease.setPercentage(20);
		return priceIncrease;
	}

	public ProductManager getProductManager() {
		return productManager;
	}

	public void setProductManager(ProductManager productManager) {
		this.productManager = productManager;
	}
}
