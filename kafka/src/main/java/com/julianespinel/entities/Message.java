package com.julianespinel.entities;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Message {

    private final int id;
    private final String text;

    @JsonCreator
    public Message(@JsonProperty("id") int id, @JsonProperty("text") String text) {
        this.id = id;
        this.text = text;
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
}