package lab.sharding.orders;

import java.time.LocalDateTime;

public record Order(long id, int items, int priceInUSD, LocalDateTime creationDate) {
}
