package lab.sharding.orders;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class OrderFactory {

    private final Random random;

    public OrderFactory(Random random) {
        this.random = random;
    }

    public Order generateRandomOrder(long id) {
        LocalDateTime randomDate = LocalDateTime.now()
                .minusYears(random.nextInt(100))
                .minusMonths(random.nextInt(12))
                .minusDays(random.nextInt(30));
        int randomItems = random.nextInt(1,30);
        int randomPriceInUSD = randomItems * random.nextInt(1, 100);
        return new Order(id, randomItems, randomPriceInUSD, randomDate);
    }

    public List<Order> generateRandomOrders(int size) {
        List<Order> orders = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            var order = generateRandomOrder(i + 1);
            orders.add(order);
        }
        return orders;
    }
}
