package lab.sharding.orders;

import lab.sharding.common.DBTest;
import lab.sharding.infrastructure.Databases;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import javax.sql.DataSource;
import java.util.List;
import java.util.Map;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.assertEquals;

class OrderRepositoryTest extends DBTest {

    private static OrderFactory factory;
    private static OrderRepository repository;

    @BeforeEach
    void setUp() {
        factory = new OrderFactory(new Random());

        List<DataSource> dataSources = Databases.getDataSources(dbContainers);
        Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc = Databases.getShardingKeyToJdbc(dataSources);
        Databases.createTables(shardingKeyToJdbc.values());
        repository = new OrderRepository(shardingKeyToJdbc);
    }

    @Test
    void testInsertOrdersOneByOne_createOrdersInShards() {
        // arrange
        int size = 9;
        List<Order> orders = factory.generateRandomOrders(size);
        // act
        repository.insertOrdersOneByOne(orders);
        // assert
        int ordersPerShard = size / repository.getNumberOfShards();
        assertEquals(ordersPerShard, repository.countOrdersInShard(0));
        assertEquals(ordersPerShard, repository.countOrdersInShard(1));
        assertEquals(ordersPerShard, repository.countOrdersInShard(2));
    }

    @Test
    void testInsertOrdersInBatch_createOrdersInShards() {
        // arrange
        int size = 9;
        List<Order> orders = factory.generateRandomOrders(size);
        // act
        repository.insertOrdersInBatch(orders);
        // assert
        int ordersPerShard = size / repository.getNumberOfShards();
        assertEquals(ordersPerShard, repository.countOrdersInShard(0));
        assertEquals(ordersPerShard, repository.countOrdersInShard(1));
        assertEquals(ordersPerShard, repository.countOrdersInShard(2));
    }

    @Test
    void testDeleteAll_removeOrdersFromShards() {
        // act
        repository.deleteAll();
        // assert
        assertEquals(0, repository.countOrdersInShard(0));
        assertEquals(0, repository.countOrdersInShard(1));
        assertEquals(0, repository.countOrdersInShard(2));
    }
}
