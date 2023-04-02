package lab.sharding.infrastructure;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.command.PullImageResultCallback;
import com.github.dockerjava.api.command.WaitContainerResultCallback;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientConfig;
import com.github.dockerjava.core.DockerClientImpl;
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient;
import com.github.dockerjava.transport.DockerHttpClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class Containers {

    private static final Logger logger = LogManager.getLogger(Containers.class);

    public static final String DOCKER_HOST = "unix:///var/run/docker.sock";

    public static final String POSTGRES_DRIVER_CLASS_NAME = "org.postgresql.Driver";
    public static final String POSTGRES_IMAGE = "postgres:15-alpine";
    private static final int POSTGRES_PORT = 5432;
    private static final String POSTGRES_JDBC_URL = "jdbc:postgresql://localhost:%s/%s";

    private final DockerClient dockerClient;

    public Containers(DockerClient dockerClient) {
        this.dockerClient = dockerClient;
    }

    public static DockerClient initializeDockerClient() {
        DockerClientConfig config = DefaultDockerClientConfig.createDefaultConfigBuilder()
                .withDockerHost(DOCKER_HOST)
                .build();

        DockerHttpClient httpClient = new ApacheDockerHttpClient.Builder()
                .dockerHost(config.getDockerHost())
                .sslConfig(config.getSSLConfig())
                .maxConnections(100)
                .connectionTimeout(Duration.ofSeconds(30))
                .responseTimeout(Duration.ofSeconds(45))
                .build();

        return DockerClientImpl.getInstance(config, httpClient);
    }

    public static void waitForPostgresContainersToBeReady(List<DBContainer> dbContainers) throws InterruptedException {
        for (DBContainer dbContainer : dbContainers) {
            while (!containerIsReady(dbContainer)) {
                logger.error(String.format("Container %s is not ready", dbContainer.containerId()));
                Thread.sleep(Duration.ofMillis(300));
            }
            logger.info(String.format("Container %s is ready", dbContainer.containerId()));
        }
    }

    private static boolean containerIsReady(DBContainer dbContainer) {
        try {
            DataSource dataSource = Databases.getDataSource(dbContainer);
            Connection connection = dataSource.getConnection();
            boolean isOpen = !connection.isClosed();
            if (isOpen) {
                connection.close();
            }
            return isOpen;
        } catch (SQLException e) {
            return false;
        }
    }

    public List<DBContainer> createDatabasesContainers(
            int databasesNumber,
            String databaseName,
            String username,
            String password
    ) throws InterruptedException {
        List<DBContainer> containers = new ArrayList<>(databasesNumber);

        dockerClient.pullImageCmd(POSTGRES_IMAGE)
                .exec(new PullImageResultCallback())
                .awaitCompletion(30, TimeUnit.SECONDS);

        for (int i = 0; i < databasesNumber; i++) {
            int hostPort = (10_000 + i) + POSTGRES_PORT;
            String HostPortToContainerPort = String.format("%s:%s", hostPort, POSTGRES_PORT);

            CreateContainerResponse response = dockerClient.createContainerCmd(POSTGRES_IMAGE)
                    .withPortBindings(PortBinding.parse(HostPortToContainerPort))
                    .withEnv(
                            String.format("POSTGRES_DB=%s", databaseName),
                            String.format("POSTGRES_USER=%s", username),
                            String.format("POSTGRES_PASSWORD=%s", password)
                    )
                    .exec();

            String containerId = response.getId();
            dockerClient.startContainerCmd(containerId).exec();
            dockerClient.waitContainerCmd(containerId).exec(new WaitContainerResultCallback());

            String jdbcUrl = String.format(POSTGRES_JDBC_URL, hostPort, databaseName);
            DBContainer container = new DBContainer(containerId, POSTGRES_DRIVER_CLASS_NAME, jdbcUrl, username, password);
            containers.add(container);
        }

        return containers;
    }

    public void shutdownDatabaseContainers(List<DBContainer> dbContainers, int waitingTimeInSeconds) {
        try {
            Thread.sleep(Duration.ofSeconds(waitingTimeInSeconds));
            for (DBContainer container : dbContainers) {
                String containerId = container.containerId();
                dockerClient.stopContainerCmd(containerId).exec();
                dockerClient.removeContainerCmd(containerId).exec();
            }
        } catch (InterruptedException e) {
            logger.error(e);
        }
    }
}
