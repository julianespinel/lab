package com.julianespinel.kafka;

import com.fasterxml.jackson.databind.JsonNode;
import com.julianespinel.entities.Message;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.LongDeserializer;

public class Consumer {

    private final KafkaConsumer<Long, JsonNode> kafkaConsumer;

    public Consumer(KafkaConsumer<Long, JsonNode> kafkaConsumer) {
        this.kafkaConsumer = kafkaConsumer;
    }

    public static Consumer getConsumer(String serverIPAndPort) {
        Properties properties = new Properties();
        properties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, serverIPAndPort);
        properties.put(ConsumerConfig.GROUP_ID_CONFIG, "kafka_consumer");
        properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, LongDeserializer.class.getName());
        properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, JsonDeserializer.class.getName());
        KafkaConsumer<Long, JsonNode> kafkaConsumer = new KafkaConsumer<>(properties);
        return new Consumer(kafkaConsumer);
    }

    public void receiveMessages(String topicName) {
        List<Message> messages = new ArrayList<>();
        kafkaConsumer.subscribe(Collections.singletonList(topicName));
        Duration timout = Duration.ofMillis(1_000);
        while (true) {
            ConsumerRecords<Long, JsonNode> records = kafkaConsumer.poll(timout);
            if (records.isEmpty()) {
                continue;
            }
            for (ConsumerRecord<Long, JsonNode> record : records) {
                Long key = record.key();
                JsonNode jsonNode = record.value();
                Message message = Message.from(jsonNode);
                int partition = record.partition();
                long offset = record.offset();
                System.out.printf("Consumer Record:(%d, %s, %d, %d)\n", key, message, partition, offset);
                messages.add(message);
            }
            kafkaConsumer.commitAsync();
            break;
        }
        kafkaConsumer.close();
    }
}
