package model.classDiagram;

import model.sequenceDiagramParser.Meta;
import java.util.List;

/**
 * Stores fetchable data used for the GSON parser.
 * @author Pontus Laestadius
 * @version 1.0
 */
public class ClassDiagram {
    public Meta meta;
    public String type;
    public List<Relationships> relationships;
    public List<Classes> classes;
}
