package com.julianespinel.entities;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class Message {

    private final int id;
    private final String text;

    @JsonCreator
    public Message(@JsonProperty("id") int id, @JsonProperty("text") String text) {
        this.id = id;
        this.text = text;
    }

    public static Message from(JsonNode jsonNode) {
        return new Message(
            jsonNode.get("id").asInt(),
            jsonNode.get("text").asText()
        );
    }

    public int getId() {
        return id;
    }

    public String getText() {
        return text;
    }

    @Override
    public String toString() {
        return "id: " + id + ", text: " + text;
    }

    public JsonNode toJson() {
        ObjectNode node = JsonNodeFactory.instance.objectNode();
        node.put("id", id);
        node.put("text", text);
        return node;
    }
}