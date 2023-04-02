package lab.sharding.orders;

import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.jdbc.core.namedparam.SqlParameterSourceUtils;

import java.util.*;

public class OrderRepository {

    private final String INSERT_ORDER_SQL = """
            INSERT INTO orders (id, items, price_in_usd, creation_date)
            VALUES (:id, :items, :priceInUSD, :creationDate);
            """;

    private static final String DELETE_ORDERS_SQL = "DELETE FROM orders;";

    private static final String COUNT_ORDERS_SQL = "SELECT COUNT(*) FROM orders;";

    private final int numberOfShards;
    private final Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc;

    public OrderRepository(Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc) {
        this.shardingKeyToJdbc = shardingKeyToJdbc;
        this.numberOfShards = shardingKeyToJdbc.size();
    }

    public int getNumberOfShards() {
        return numberOfShards;
    }

    public void insertOrdersOneByOne(List<Order> orders) {
        // Not to be done in prod, done here for educative purposes
        for (Order order : orders) {
            long shardingKey = order.id() % numberOfShards;
            var jdbcTemplate = shardingKeyToJdbc.get(shardingKey);

            SqlParameterSource params = new MapSqlParameterSource()
                    .addValue("id", order.id())
                    .addValue("items", order.items())
                    .addValue("priceInUSD", order.priceInUSD())
                    .addValue("creationDate", order.creationDate());

            jdbcTemplate.update(INSERT_ORDER_SQL, params);
        }
    }

    public void insertOrdersInBatch(List<Order> orders) {
        Map<Long, List<Order>> shardToOrders = getShardingKeyToOrdersMap(orders);
        for (long shardingKey : shardToOrders.keySet()) {
            var jdbcTemplate = shardingKeyToJdbc.get(shardingKey);
            List<Order> shardOrders = shardToOrders.get(shardingKey);
            SqlParameterSource[] params = SqlParameterSourceUtils.createBatch(shardOrders);
            jdbcTemplate.batchUpdate(INSERT_ORDER_SQL, params);
        }
    }

    private Map<Long, List<Order>> getShardingKeyToOrdersMap(List<Order> orders) {
        Map<Long, List<Order>> shardToOrders = new HashMap<>();
        for (Order order : orders) {
            long shardingKey = order.id() % numberOfShards;
            List<Order> shardOrders = shardToOrders.getOrDefault(shardingKey, new ArrayList<>());
            shardOrders.add(order);
            shardToOrders.put(shardingKey, shardOrders);
        }
        return shardToOrders;
    }

    public void deleteAll() {
        for (NamedParameterJdbcTemplate jdbcTemplate : shardingKeyToJdbc.values()) {
            jdbcTemplate.getJdbcTemplate().execute(DELETE_ORDERS_SQL);
        }
    }

    public int countOrdersInShard(long shardingKey) {
        NamedParameterJdbcTemplate jdbcTemplate = shardingKeyToJdbc.get(shardingKey);
        return jdbcTemplate.queryForObject(COUNT_ORDERS_SQL, Collections.emptyMap(), Integer.class);
    }
}
