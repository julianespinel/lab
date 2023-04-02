package lab.sharding.infrastructure;

import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;

import javax.sql.DataSource;
import java.util.*;

public class Databases {

    private static final String CREATE_ORDERS_TABLE_SQL = """
                CREATE TABLE orders (
                    id            BIGSERIAL PRIMARY KEY,
                    items         INTEGER,
                    price_in_usd  INTEGER,
                    creation_date TIMESTAMP
                );
            """;

    private static final String DROP_ORDERS_TABLE_SQL = "DROP TABLE IF EXISTS orders;";

    public static List<DataSource> getDataSources(List<DBContainer> containers) {
        List<DataSource> dataSources = new ArrayList<>();
        for (DBContainer container : containers) {
            DataSource dataSource = getDataSource(container);
            dataSources.add(dataSource);
        }
        return dataSources;
    }

    public static DataSource getDataSource(DBContainer container) {
        DataSource dataSource = DataSourceBuilder.create()
                .driverClassName(container.driverClassName())
                .url(container.url())
                .username(container.username())
                .password(container.password())
                .build();
        return dataSource;
    }

    public static void createTables(Collection<NamedParameterJdbcTemplate> jdbcTemplates) {
        for (NamedParameterJdbcTemplate jdbcTemplate : jdbcTemplates) {
            jdbcTemplate.getJdbcTemplate().execute(CREATE_ORDERS_TABLE_SQL);
        }
    }

    public static void dropTables(List<DBContainer> dbContainers) {
        List<DataSource> dataSources = Databases.getDataSources(dbContainers);
        Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc = Databases.getShardingKeyToJdbc(dataSources);
        for (NamedParameterJdbcTemplate jdbcTemplate : shardingKeyToJdbc.values()) {
            jdbcTemplate.getJdbcTemplate().execute(DROP_ORDERS_TABLE_SQL);
        }
    }

    public static Map<Long, NamedParameterJdbcTemplate> getShardingKeyToJdbc(
            List<DataSource> dataSources) {

        Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc = new HashMap<>();
        for (int i = 0; i < dataSources.size(); i++) {
            DataSource dataSource = dataSources.get(i);
            NamedParameterJdbcTemplate jdbcTemplate = new NamedParameterJdbcTemplate(dataSource);
            shardingKeyToJdbc.put((long) i, jdbcTemplate);
        }
        return shardingKeyToJdbc;
    }
}
