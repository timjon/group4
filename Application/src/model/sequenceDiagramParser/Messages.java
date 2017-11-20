package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part of the Sequence Diagram Object for the parser from JSON to POJO (Plain Old Java Objects)
 * The getters and setters and their declarations are made to adhere with a predetermined JSON format
 * This Messages class contains the messages being sent between the classes of the parsed sequence diagram
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.0
 * @since 2017-10-16
 */


public class Messages {

    private String node;

    private String from;

    private String to;

    private List<String> message = null;

    /**
     * @return current node
     */
    public String getNode() {
        return node;
    }

    /**
     * @param node to set
     */
    public void setNode(String node) {
        this.node = node;
    }

    /**
     * @return current from (message sender)
     */
    public String getFrom() {
        return from;
    }

    /**
     * @param from to set
     */
    public void setFrom(String from) {
        this.from = from;
    }

    /**
     * @return current to (message receiver)
     */
    public String getTo() {
        return to;
    }

    /**
     * @param to to set
     */
    public void setTo(String to) {
        this.to = to;
    }

    /**
     * @return message (message that is being sent)
     */
    public List<String> getMessage() {
        return message;
    }

    /**
     * @param message to set
     */
    public void setMessage(List<String> message) {
        this.message = message;
    }

}