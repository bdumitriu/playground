import javax.annotation.Resource;
import javax.ejb.AsyncResult;
import javax.ejb.Asynchronous;
import javax.ejb.SessionContext;
import javax.ejb.Stateless;
import java.util.concurrent.Future;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@Asynchronous
public class OrderEJB {

	@Resource
	private SessionContext sessionContext;

	private void sendEmailOrderComplete(Order order, Customer customer) {
		// send email
	}

	private void printOrder(Order order) {
		// print order
	}

	private Future<Integer> sendOrderToWorkflow(Order order) {
		Integer status = 2;
		// processing
		status = 1;
		if (sessionContext.wasCancelCalled()) {
			return new AsyncResult<Integer>(2);
		}
		// processing
		return new AsyncResult<Integer>(status);
	}
}
