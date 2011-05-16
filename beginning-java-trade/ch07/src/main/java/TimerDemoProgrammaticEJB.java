import javax.annotation.Resource;
import javax.ejb.*;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * Demos setting up timers automatically.
 *
 * @author Bogdan Dumitriu
 */
@Stateless
public class TimerDemoProgrammaticEJB {

	@Resource
	TimerService timerService;

	@PersistenceContext(unitName = "chapter07PU")
	private EntityManager entityManager;

	public void createCustomer(Customer customer) {
		entityManager.persist(customer);
		final ScheduleExpression birthday =
				new ScheduleExpression().dayOfMonth(customer.getBirthDay()).dayOfMonth(customer.getBirthMonth());
		timerService.createCalendarTimer(birthday, new TimerConfig(customer, true));
	}

	@Timeout
	public void sendBirthdayEmail(Timer timer) {
		final Customer customer = (Customer) timer.getInfo();
		// send email...
	}
}
