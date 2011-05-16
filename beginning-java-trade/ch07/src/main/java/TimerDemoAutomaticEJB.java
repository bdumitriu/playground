import javax.ejb.Schedule;
import javax.ejb.Schedules;
import javax.ejb.Stateless;

/**
 * Demos setting up timers automatically.
 *
 * @author Bogdan Dumitriu
 */
@Stateless
public class TimerDemoAutomaticEJB {

	@Schedule(second = "*/10", minute = "*", hour = "*")
	public void runEveryFiveSeconds() {
		// System.out.println("La semnalul urmator va fi ora " + new Date());
	}

	@Schedules({
			@Schedule(hour = "2"),
			@Schedule(hour = "14", dayOfWeek = "Wed")
	})
	public void runOnSomeSchedule() {
		// ...
	}

	@Schedule(minute = "*/10", hour = "*", persistent = false)
	public void runEveryTenMinutesUntilTheJVMIsClosed() {
		// ...
	}
}
