package tacos;

import java.util.ArrayList;
import java.util.List;

public record TacoOrder(
    String deliveryName,
    String deliveryStreet,
    String deliveryZip,
    String deliveryCity,
    String deliveryCountry,
    String ccNumber,
    String ccExpiration,
    String ccCVV,
    List<Taco> tacos
) {
    public TacoOrder {
        tacos = new ArrayList<>();
    }

    public TacoOrder() {
        this(null, null, null, null, null, null, null, null, null);
    }

    public void addTaco(Taco taco) {
        tacos.add(taco);
    }
}
