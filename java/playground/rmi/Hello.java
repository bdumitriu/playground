package RMI.hello;

public interface Hello extends java.rmi.Remote
{
	String sayHello() throws java.rmi.RemoteException;
	String polyglotGreetings(String lang) throws java.rmi.RemoteException;
}
