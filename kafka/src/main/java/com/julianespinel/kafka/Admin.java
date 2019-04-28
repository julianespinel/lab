package com.julianespinel.kafka;

import java.util.Collections;
import java.util.Properties;
import org.apache.kafka.clients.admin.AdminClient;
import org.apache.kafka.clients.admin.NewTopic;
import org.apache.kafka.clients.producer.ProducerConfig;

public class Admin {

    public static void createTopic(String serverIPAndPort, String topicName) {
        Properties properties = new Properties();
        properties.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, serverIPAndPort);
        AdminClient adminClient = AdminClient.create(properties);
        NewTopic topic = new NewTopic(topicName, 1, (short) 1);
        adminClient.createTopics(Collections.singletonList(topic));
        adminClient.close();
    }
}
