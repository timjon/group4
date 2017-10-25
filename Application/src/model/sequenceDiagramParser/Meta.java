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

public class Meta {

    private String format;

    private String version;

    private List<Object> extensions = null;

    private void setExtensions(List<Object> extensions) {
        this.extensions = extensions;
    }

    // getters and setters

    public List<Object> getExtensions() {
        return extensions;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }


}