package lab.sharding.common;

import com.github.dockerjava.api.DockerClient;
import lab.sharding.infrastructure.Containers;
import lab.sharding.infrastructure.DBContainer;
import lab.sharding.infrastructure.Databases;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;

import java.sql.SQLException;
import java.util.List;

/**
 * Class to use database(s) within a container(s) for tests.
 */
public class DBTest {

    public static final int DATABASES_NUMBER = 3;
    public static final String DB_NAME = "test_db";
    public static final String USERNAME = "username";
    public static final String PASSWORD = "password";

    public static Containers containers;
    public static List<DBContainer> dbContainers;

    @BeforeAll
    static void beforeAll() throws InterruptedException, SQLException {
        DockerClient dockerClient = Containers.initializeDockerClient();
        containers = new Containers(dockerClient);
        dbContainers = containers.createDatabasesContainers(
                DATABASES_NUMBER, DB_NAME, USERNAME, PASSWORD
        );

        Containers.waitForPostgresContainersToBeReady(dbContainers);
    }

    @AfterEach
    void tearDown() {
        Databases.dropTables(dbContainers);
    }

    @AfterAll
    static void afterAll() {
        containers.shutdownDatabaseContainers(dbContainers, 0);
    }
}
