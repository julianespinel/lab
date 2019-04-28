package com.julianespinel;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.julianespinel.entities.Message;
import com.julianespinel.kafka.Admin;
import com.julianespinel.kafka.Consumer;
import com.julianespinel.kafka.Producer;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class App {

    private static List<Message> generateRandomMessages(int size) {
        List<Message> messages = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            int id = i + 1;
            String text = "Generated string " + id;
            Message message = new Message(id, text);
            messages.add(message);
        }
        return messages;
    }

    public static void main(String[] args) throws InterruptedException {
        String serverIPAndPort = "localhost:9092";
        String topicName = "test_topic";
        Admin.createTopic(serverIPAndPort, topicName);

        int size = 100;
        List<Message> messages = generateRandomMessages(size);

        ObjectMapper mapper = new ObjectMapper();
        List<JsonNode> messagesAsJson = messages.stream()
            .map(message -> message.toJson())
            .collect(Collectors.toList());

        Producer producer = Producer.getProducer(serverIPAndPort);
        producer.publishMessages(topicName, messagesAsJson);

        Consumer consumer = Consumer.getConsumer(serverIPAndPort);
        consumer.receiveMessages(topicName);
    }
}
