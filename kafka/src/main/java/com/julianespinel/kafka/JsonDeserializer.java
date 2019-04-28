package com.julianespinel.kafka;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Map;
import org.apache.kafka.common.serialization.Deserializer;

public class JsonDeserializer implements Deserializer<JsonNode> {

    private final ObjectMapper mapper = new ObjectMapper();

    @Override
    public void configure(Map<String, ?> configs, boolean isKey) {
    }

    @Override
    public JsonNode deserialize(String topic, byte[] data) {
        JsonNode jsonNode = JsonNodeFactory.instance.objectNode();
        try {
            jsonNode = mapper.readTree(new ByteArrayInputStream(data));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return jsonNode;
    }

    @Override
    public void close() {
    }
}
