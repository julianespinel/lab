package lab.sharding.orders;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

class OrderFactoryTest {

    private final Random random = new Random();
    private final OrderFactory factory = new OrderFactory(random);

    @Test
    void testGenerateRandomOrder_returnsRandomOrder() {
        // arrange
        int orderId = 1;
        // act
        Order order = factory.generateRandomOrder(orderId);
        // assert
        validateRandomOrder(orderId, order);
    }

    @Test
    void testGenerateRandomOrders_returnsListOfRandomOrders() {
        // arrange
        int size = 10;
        // act
        List<Order> orders = factory.generateRandomOrders(size);
        // assert
        assertEquals(orders.size(), size);
        validateMultipleRandomOrders(orders);
    }

    private static void validateRandomOrder(int orderId, Order order) {
        assertEquals(order.id(), orderId);
        assertNotEquals(order.items(), 0);
        assertNotEquals(order.priceInUSD(), 0);
        assertNotEquals(order.creationDate(), null);
    }

    private static void validateMultipleRandomOrders(List<Order> orders) {
        for (int i = 0; i < orders.size(); i++) {
            validateRandomOrder(i + 1, orders.get(i));
        }
    }
}
