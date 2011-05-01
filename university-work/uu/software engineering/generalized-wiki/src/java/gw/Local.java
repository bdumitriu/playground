package gw;

public class Local {
	private static class ThreadLocalConnection extends ThreadLocal {
		public Object initialValue() {
		     return new Context();
		}
	}	
    private static ThreadLocalConnection sessionContextHolder = new ThreadLocalConnection();
	
	public static GwSessionContext getSessionContext(){
		return ((Context) sessionContextHolder.get()).getSessioncontext();
	}
	public static void setSessionContext(GwSessionContext context){
		((Context) sessionContextHolder.get()).setSessioncontext(context);
	}
	public static GwContext getGwContext(){
		return ((Context) sessionContextHolder.get()).get_context();
	}
	public static void setGwContext(GwContext context){
		((Context) sessionContextHolder.get()).set_context(context);
	}
}

class Context{
	private GwContext _context;
	private GwSessionContext sessioncontext;
	
	public GwContext get_context() {
		return _context;
	}
	public void set_context(GwContext _context) {
		this._context = _context;
	}
	public GwSessionContext getSessioncontext() {
		return sessioncontext;
	}
	public void setSessioncontext(GwSessionContext sessioncontext) {
		this.sessioncontext = sessioncontext;
	}
	
	
}


