package gw.util;

import java.io.IOException;
import java.io.PrintStream;
import sun.net.smtp.SmtpClient;

/**
 * Generalized e-mail messaging engine.
 */
public class SmtpMailer {
	
	private String	_smtpServer;
	private String  _emailTo;
	private String  _emailFrom;
	private String	_subject;
	private String 	_message;	

	public SmtpMailer( String smtpServer, String emailTo, String emailFrom, String subject
					 , String message ) {					 	
		setSmtpServer( smtpServer );
		setEmailTo( emailTo );
		setEmailFrom( emailFrom );
		setSubject( subject );
		setMessage( message );
	}

	/**
     * @return Returns the from address.
	 */
    public String getEmailFrom() {		
		return _emailFrom;
	}

	/**
     * @return Returns the to address.
	 */
	public String getEmailTo() {		
		return _emailTo;
	}

	/**
	 * @return Returns the message.
	 */
	public String getMessage() {
		return _message;
	}

	/**
	 * @return Returns the address of the SMTP server.
	 */
	public String getSmtpServer() {		
		return _smtpServer;
	}

	/**
	 * @return Returns the subject.
	 */
	public String getSubject() {		
		return _subject;
	}

	/**
	 * @param emailFrom The from address.
	 */
	public void setEmailFrom(String emailFrom) {		
		if(emailFrom == null)
					throw new NullPointerException( "Given email address (From) can not be null" );
					
		_emailFrom = emailFrom;
	}

	/**
	 * @param emailTo The to address.
	 */
	public void setEmailTo(String emailTo ) {		
		if(emailTo == null)
					throw new NullPointerException( "Given email address (To) can not be null" );
		
		_emailTo = emailTo;
	}

	/**
	 * @param message The message.
	 */
	public void setMessage(String message) {		
		if(message == null)
			throw new NullPointerException( "Given message can not be null" );
			
		_message = message;
	}

	/**
	 * @param server The address of the smtp server.
	 */
	public void setSmtpServer(String server) {		
		if(server == null)
					throw new NullPointerException( "Given server can not be null" );
					
		_smtpServer = server;
	}

	/**
	 * @param subject The subject.
	 */
	public void setSubject(String subject) {		
		if(subject == null)
					throw new NullPointerException( "Given subject can not be null" );
					
		_subject = subject;
	}
	
    /**
     * Sends the message.
     * @throws IOException
     */
	public void send( ) throws IOException {		
		SmtpClient mailClient	= new SmtpClient( getSmtpServer() );
		mailClient.from( getEmailFrom() );
		mailClient.to( getEmailTo() );
		PrintStream messageStream	= mailClient.startMessage();

		messageStream.println( "Subject: " + getSubject() );
		messageStream.println();
		messageStream.println( getMessage() );
		messageStream.flush();
		
		mailClient.closeServer();
	}
}
