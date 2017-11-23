package model.classDiagram;

/**
 * Stores fetchable data used for the GSON parser.
 * @author Pontus Laestadius
 * @version 1.0
 */
public class Relationships {
    public String type;
    public String superclass;
    public String subclass;

    /**
     * @return a formatted string.
     */
    public String format() {
        return "[" + type + "," + superclass + "," + subclass + "]";
    }
}
