package ducttape.managers;

import ducttape.entities.WebOrder;

import javax.ejb.Asynchronous;
import javax.ejb.Stateless;
import javax.enterprise.event.Observes;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
public class SmsNotifier {

    @Asynchronous // only works if @Stateless if also present (perhaps others as well, but not with an unannotated class
    public void sendSMS(@Observes WebOrder webOrderEvent) {
        try {
            Thread.sleep(5000);
        } catch (InterruptedException ignored) {
        }

        System.out.println("Sending to " + webOrderEvent.getCustomer().getName());
    }
}
