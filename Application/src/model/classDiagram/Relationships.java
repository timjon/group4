package model.classDiagram;

/**
 * Stores fetch-able data used for the GSON parser.
 * @author Pontus Laestadius
 * @version 1.1
 */
public class Relationships {
    private String type;
    private String superclass;
    private String subclass;

    /**
     * @return a formatted string.
     */
    public String format() {
        return "['" + type + "','" + superclass + "','" + subclass + "']";
    }
}
