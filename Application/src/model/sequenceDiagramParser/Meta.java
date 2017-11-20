package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part of the Sequence Diagram Object for the parser from JSON to POJO (Plain Old Java Objects)
 * The getters and setters and their declarations are made to adhere with a predetermined JSON format
 * This Meta class contains the information of the parsed diagram including the name of the format, the version and
 * the list of extensions
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.0
 * @since 2017-10-16
 */

public class Meta {

    private String format;

    private String version;

    private List<Object> extensions = null;


    /**
     * @return current object List of extensions
     */
    public List<Object> getExtensions() {
        return extensions;
    }

    /**
     * @param extensions to set
     */
    private void setExtensions(List<Object> extensions) {
        this.extensions = extensions;
    }

    /**
     * @return current format
     */
    public String getFormat() {
        return format;
    }

    /**
     * @param format to set
     */
    public void setFormat(String format) {
        this.format = format;
    }

    /**
     * @return current version
     */
    public String getVersion() {
        return version;
    }

    /**
     * @param version to set
     */
    public void setVersion(String version) {
        this.version = version;
    }


}