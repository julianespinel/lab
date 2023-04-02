package lab.sharding;

import ch.qos.logback.classic.Level;
import com.github.dockerjava.api.DockerClient;
import com.google.common.base.Stopwatch;
import lab.sharding.infrastructure.Containers;
import lab.sharding.infrastructure.DBContainer;
import lab.sharding.infrastructure.Databases;
import lab.sharding.infrastructure.Logs;
import lab.sharding.orders.Order;
import lab.sharding.orders.OrderFactory;
import lab.sharding.orders.OrderRepository;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import picocli.CommandLine;

import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.TimeUnit;

/**
 * This is the entry point of the application.
 * Please refer to the README to see how to execute it.
 */
@CommandLine.Command(
        name = "shard",
        description = "Execute a database sharding experiment"
)
public class ShardCommand implements Runnable {

    private static final Logger logger = LogManager.getLogger(ShardCommand.class);

    private static final String DB_NAME = "sharding_lab_db";
    private static final String USERNAME = "username";
    private static final String PASSWORD = "password";

    @CommandLine.Option(
            description = "Number of shards (databases) we want to create",
            names = {"-s", "--shard"},
            required = true
    )
    private int shardsNumber;

    @CommandLine.Option(
            description = "Number of orders we want to create",
            names = {"-o", "--orders"},
            required = true
    )
    private int ordersNumber;

    @CommandLine.Option(names = {"-h", "--help"}, usageHelp = true)
    boolean usageHelpRequested;

    @CommandLine.Option(
            description = "Number seconds we want to wait after all operations " +
                    "are executed and before all the containers are killed. " +
                    "Useful if you want to get access to the database in the " +
                    "containers and check how the data was distributed among" +
                    "the shards.",
            names = {"-wts", "--waiting-time-seconds"},
            defaultValue = "0"
    )
    private int waitingTimeInSeconds;

    public static void main(String[] args) {
        Logs.setDependenciesLoggingToLevel(Level.WARN);
        int exitCode = new CommandLine(new ShardCommand()).execute(args);
        System.exit(exitCode);
    }

    @Override
    public void run() {
        DockerClient dockerClient = Containers.initializeDockerClient();
        Containers containers = new Containers(dockerClient);
        List<DBContainer> dbContainers = new ArrayList<>();

        try {
            dbContainers = containers.createDatabasesContainers(
                    shardsNumber, DB_NAME, USERNAME, PASSWORD
            );

            Containers.waitForPostgresContainersToBeReady(dbContainers);

            List<DataSource> dataSources = Databases.getDataSources(dbContainers);
            Map<Long, NamedParameterJdbcTemplate> shardingKeyToJdbc = Databases.getShardingKeyToJdbc(dataSources);
            Databases.createTables(shardingKeyToJdbc.values());

            OrderFactory factory = new OrderFactory(new Random());
            OrderRepository repository = new OrderRepository(shardingKeyToJdbc);
            List<Order> orders = factory.generateRandomOrders(ordersNumber);

            Stopwatch stopwatch = Stopwatch.createUnstarted();
            stopwatch.start();
            repository.insertOrdersOneByOne(orders);
            stopwatch.stop();
            stopwatch.elapsed(TimeUnit.MILLISECONDS);
            logger.info("insertOrdersOneByOne: time: " + stopwatch);

            repository.deleteAll();
            stopwatch.reset();

            stopwatch.start();
            repository.insertOrdersInBatch(orders);
            stopwatch.stop();
            stopwatch.elapsed(TimeUnit.MILLISECONDS);
            logger.info("insertOrdersInBatch: time: " + stopwatch);
        } catch (InterruptedException e) {
            logger.error(e);
        } finally {
            containers.shutdownDatabaseContainers(dbContainers, waitingTimeInSeconds);
        }
    }
}
