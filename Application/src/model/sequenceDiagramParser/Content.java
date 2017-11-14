package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part of the Sequence Diagram Object for the parser from JSON to POJO (Plain Old Java Objects)
 * The getters and setters and their declarations are made to adhere with a predetermined JSON format
 * This Content class will include the content of the parsed diagram, including the main node and the list of messages
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.0
 * @since 2017-10-16
 */


public class Content {

    private String node;

    private List<Messages> content = null;

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
     * @return current list of contentArray (referencing to Messages class)
     */
    public List<Messages> getMessages() {
        return content;
    }

    /**
     * @param content to set
     */
    public void setMessages(List<Messages> content) {
        this.content = content;
    }

}