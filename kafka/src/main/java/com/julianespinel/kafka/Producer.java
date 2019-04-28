package com.julianespinel.kafka;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.Properties;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.common.serialization.LongSerializer;

public class Producer {

    private final KafkaProducer<Long, JsonNode> kafkaProducer;

    public Producer(KafkaProducer<Long, JsonNode> kafkaProducer) {
        this.kafkaProducer = kafkaProducer;
    }

    public static Producer getProducer(String serverIPAndPort) {
        Properties properties = new Properties();
        properties.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, serverIPAndPort);
        properties.put(ProducerConfig.CLIENT_ID_CONFIG, "kafka_producer");
        properties.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, LongSerializer.class.getName());
        properties.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, JsonSerializer.class.getName());
        KafkaProducer<Long, JsonNode> kafkaProducer = new KafkaProducer<>(properties);
        return new Producer(kafkaProducer);
    }

    public void publishMessages(String topicName, List<JsonNode> messages) {
        for (JsonNode message : messages) {
            ProducerRecord<Long, JsonNode> record = new ProducerRecord<Long, JsonNode>(topicName, message);
            kafkaProducer.send(record);
        }
        kafkaProducer.flush();
        kafkaProducer.close();
    }
}
