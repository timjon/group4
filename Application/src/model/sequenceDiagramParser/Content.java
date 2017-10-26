package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part Sequence Diagram Object for the parser from JSON to POJO (Plain Old Java Objects)
 * The getters and setters and their decelerations are made to adhere with a predetermined JSON format
 *
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.0
 * @since 2017-10-16
 */


public class Content {


    private String node;

    private List<ContentArray> content = null;

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
     * @return current list of contentArray (referencing to ContentArray class)
     */
    public List<ContentArray> getContent() {
        return content;
    }

    /**
     * @param content to set
     */
    public void setContent(List<ContentArray> content) {
        this.content = content;
    }

}