import static javax.persistence.EnumType.STRING;

import javax.persistence.*;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "credit_card")
public class CreditCard {

	@Id
	private String number;

	private String expiryDate;

	private Integer controlNumber;

	@Enumerated(STRING)
	private CreditCardType creditCardType;

	public String getNumber() {
		return number;
	}

	public void setNumber(String number) {
		this.number = number;
	}

	public String getExpiryDate() {
		return expiryDate;
	}

	public void setExpiryDate(String expiryDate) {
		this.expiryDate = expiryDate;
	}

	public Integer getControlNumber() {
		return controlNumber;
	}

	public void setControlNumber(Integer controlNumber) {
		this.controlNumber = controlNumber;
	}

	public CreditCardType getCreditCardType() {
		return creditCardType;
	}

	public void setCreditCardType(CreditCardType creditCardType) {
		this.creditCardType = creditCardType;
	}
}
