package springapp.web;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.Controller;

import springapp.service.ProductManager;

public class InventoryController implements Controller {

	protected final Log logger = LogFactory.getLog(getClass());

	private ProductManager productManager;

	@Override
	public ModelAndView handleRequest(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		final String now = new Date().toString();
		logger.info("Returning hello view with " + now);

		final Map<String, Object> inventoryModel = new HashMap<String, Object>();
		inventoryModel.put("now", now);
		inventoryModel.put("products", productManager.getProducts());

		return new ModelAndView("hello", "model", inventoryModel);
	}

	public void setProductManager(ProductManager productManager) {
		this.productManager = productManager;
	}
}
