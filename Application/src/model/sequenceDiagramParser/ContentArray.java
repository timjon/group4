package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part Sequence Diagram Object for the parser from JSON to POJo (Plain Old Java Objects)
 * The getters and setters and their decelerations are made to adhere with a predetermined JSON format
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @Version 1.0
 * @since 2017-10-16
 */


public class ContentArray {

    private String node;

    private String from;

    private String to;

    private List<String> message = null;

    // getters and setters

    public String getNode() {
        return node;
    }

    public void setNode(String node) {
        this.node = node;
    }

    public String getFrom() {
        return from;
    }

    public void setFrom(String from) {
        this.from = from;
    }

    public String getTo() {
        return to;
    }

    public void setTo(String to) {
        this.to = to;
    }

    public List<String> getMessage() {
        return message;
    }

    public void setMessage(List<String> message) {
        this.message = message;
    }

}