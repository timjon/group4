package model.sequenceDiagramParser;

import com.google.gson.annotations.SerializedName;

/**
 * This Class is part of the Sequence Diagram Object for the parser from JSON to POJO (Plain Old Java Objects)
 * The getters and setters and their declarations are made to adhere with a predetermined JSON format
 * This Poetesses class will contain the class names in the parsed sequence diagram
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.0
 * @since 2017-10-16
 */


public class Processes {

    /**
     * The string was named "sequenceDiagramClass" instead of "class" because the word class is protected in java,
     * the @SerializedName annotation will make the Gson library see "class" instead of "sequenceDiagramClass" when matching and parsing.
     */
    @SerializedName("class")
    private String sequenceDiagramClass;

    private String name;

    /**
     * @return current name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * The string was named "sequenceDiagramClass" instead of "class" because the word class is protected in java,
     * the @SerializedName annotation will make the Gson library see "class" instead of "sequenceDiagramClass" when matching and parsing.
     *
     * @return current class
     */
    public String getSequenceDiagramClass() {
        return sequenceDiagramClass;
    }

    /**
     * The string was named "sequenceDiagramClass" instead of "class" because the word class is protected in java,
     * the @SerializedName annotation will make the Gson library see "class" instead of "sequenceDiagramClass" when matching and parsing.
     *
     * @param sequenceDiagramClass
     */
    public void getSequenceDiagramClass(String sequenceDiagramClass) {
        this.sequenceDiagramClass = sequenceDiagramClass;
    }
}