package lab.sharding.infrastructure;

import ch.qos.logback.classic.Level;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;

public class Logs {

    public static void setDependenciesLoggingToLevel(Level level) {
        List<String> packages = Arrays.asList(
                "com.zaxxer.hikari",
                "org.apache.hc.client5",
                "org.springframework.jdbc",
                "com.github.dockerjava"
        );
        for (String aPackage : packages) {
            ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(aPackage);
            logger.setLevel(level);
        }
    }
}
