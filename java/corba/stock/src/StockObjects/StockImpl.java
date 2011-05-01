package StockObjects;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 6, 2005
 */
public class StockImpl extends StockPOA
{
	public StockImpl(String name, String description)
	{
		super();
		_description = description;
	}

	// Returns the current stock quote.
	public Quote get_quote() throws Unknown
	{
		if (_quote == null)
		{
			throw new Unknown();
		}
		return _quote;
	}

	// Sets the current stock quote.
	public void set_quote(Quote stock_quote)
	{
		_quote = stock_quote;
	}

	// e.g. company name.
	public String description()
	{
		return _description;
	}

	private Quote _quote = null;
	private String _description = null;
}
