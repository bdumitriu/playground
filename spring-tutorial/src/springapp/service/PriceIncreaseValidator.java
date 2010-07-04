package springapp.service;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

public class PriceIncreaseValidator implements Validator {

	private int DEFAULT_MIN_PERCENTAGE = 0;
	private int DEFAULT_MAX_PERCENTAGE = 50;
	private int minPercentage = DEFAULT_MIN_PERCENTAGE;
	private int maxPercentage = DEFAULT_MAX_PERCENTAGE;

	private final Log logger = LogFactory.getLog(getClass());

	public boolean supports(Class<?> clazz) {
		return PriceIncrease.class.isAssignableFrom(clazz);
	}

	@Override
	public void validate(Object target, Errors errors) {
		PriceIncrease priceIncrease = (PriceIncrease) target;
		if (priceIncrease == null) {
			errors.reject("error.not-specified", "Value required.");
		} else {
			logger.info("Validating with " + priceIncrease + ": " + priceIncrease.getPercentage());
			if (priceIncrease.getPercentage() > maxPercentage) {
				errors.rejectValue(
						"percentage", "error.too-high", new Object[] {new Integer(maxPercentage)}, "Value too high.");
			} else if (priceIncrease.getPercentage() <= minPercentage) {
				errors.rejectValue(
						"percentage", "error.too-low", new Object[] {new Integer(minPercentage)}, "Value too low.");
			}
		}
	}

	public int getMinPercentage() {
		return minPercentage;
	}

	public void setMinPercentage(int minPercentage) {
		this.minPercentage = minPercentage;
	}

	public int getMaxPercentage() {
		return maxPercentage;
	}

	public void setMaxPercentage(int maxPercentage) {
		this.maxPercentage = maxPercentage;
	}
}
