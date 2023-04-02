package lab.sharding.infrastructure;

public record DBContainer(String containerId, String driverClassName, String url, String username, String password) {
}
