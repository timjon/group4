package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part Sequence Diagram Object for the parser from JSON to POJo (Plain Old Java Objects)
 * The getters and setters and their decelerations are made to adhere with a predetermined JSON format
 *
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @Version 1.0
 * @since 2017-10-16
 */


public class Content {


    private String node;

    private List<ContentArray> content = null;

    // getters and setters

    public String getNode() {
        return node;
    }

    public void setNode(String node) {
        this.node = node;
    }

    public List<ContentArray> getContent() {
        return content;
    }

    public void setContent(List<ContentArray> content) {
        this.content = content;
    }

}