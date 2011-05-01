public class NotADirectoryException extends Exception {

	private String message;

	public NotADirectoryException(String message) {
		this.message = message;
	}

	public NotADirectoryException() {
		this("The given path was not a directory.");
	}

	public String getMessage()
	{
		return message;
	}
}