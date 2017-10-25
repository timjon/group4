package model.sequenceDiagramParser;

import com.google.gson.annotations.SerializedName;

/**
 * This Class is part Sequence Diagram Object for the parser from JSON to POJo (Plain Old Java Objects)
 * The getters and setters and their decelerations are made to adhere with a predetermined JSON format
 *
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @Version 1.0
 * @since 2017-10-16
 */


public class Processes {

    /*The string was named "sequenceDiagramClass" instead of "class" because the word class is protected in java,
    *the @SerializedName annotation will make the Gson library see "class" instead of "sequenceDiagramClass" when matching and parsing.
     */
    @SerializedName("class")
    private String sequenceDiagramClass;

    private String name;

    // getters and setters

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSequenceDiagramClass() {
        return sequenceDiagramClass;
    }

    public void getSequenceDiagramClass(String sequenceDiagramClass) {
        this.sequenceDiagramClass = sequenceDiagramClass;
    }
}