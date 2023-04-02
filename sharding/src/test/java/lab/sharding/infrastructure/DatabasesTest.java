package lab.sharding.infrastructure;

import lab.sharding.common.DBTest;
import org.junit.jupiter.api.Test;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import javax.sql.DataSource;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DatabasesTest extends DBTest {

    @Test
    void createDatabasesInDocker_DBContainersAreCreated() throws InterruptedException {
        // act: performed in the beforeAll method
        // assert
        assertEquals(dbContainers.size(), DATABASES_NUMBER);
    }

    @Test
    void getDataSources_returnsListOfDataSources() {
        // act
        List<DataSource> dataSources = Databases.getDataSources(dbContainers);
        // assert
        assertEquals(dataSources.size(), DATABASES_NUMBER);
    }

    @Test
    void createTables_doesNotThrowException() {
        // arrange
        List<DataSource> dataSources = Databases.getDataSources(dbContainers);
        Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc = Databases.getShardingKeyToJdbc(dataSources);
        // act and assert
        assertDoesNotThrow(() -> Databases.createTables(shardingKeyToJdbc.values()));
    }

    @Test
    void getShardingKeyToJdbc() {
        // arrange
        List<DataSource> dataSources = Databases.getDataSources(dbContainers);
        // act
        Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc = Databases.getShardingKeyToJdbc(dataSources);
        // assert
        assertEquals(shardingKeyToJdbc.size(), DATABASES_NUMBER);
        for (long shard = 0; shard < DATABASES_NUMBER; shard++) {
            assertTrue(shardingKeyToJdbc.containsKey(shard));
        }
    }
}
