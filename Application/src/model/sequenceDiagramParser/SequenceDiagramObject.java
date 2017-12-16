package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part of the Sequence Diagram Object for the parser from JSON to POJO (Plain Old Java Objects)
 * The getters and setters and their declarations are made to adhere with a predetermined JSON format
 *
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.1
 * @since 2017-10-16
 */

public class SequenceDiagramObject {

    //instance of class Meta to handle diagram's meta
    private Meta meta;
    //handle the Type of the diagram
    private String type;
    //instance of class Processes to handle the Processes of the diagram
    private List<Processes> processes = null;
    //instance of class Diagram to handle the messages of the diagram
    private Diagram diagram;

    /**
     * @return current meta
     */
    public Meta getMeta() {
        return meta;
    }

    /**
     * @param meta to set
     */
    public void setMeta(Meta meta) {
        this.meta = meta;
    }

    /**
     * @param type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return current type
     */
    public String getType() {
        return type;
    }

    /**
     * @param processes to set
     */
    public void setProcesses(List<Processes> processes) {
        this.processes = processes;
    }

    /**
     * @return current processes
     */
    public List<Processes> getProcesses() {
        return processes;
    }

    /**
     * @return current diagram
     */
    public Diagram getDiagram() {
        return diagram;
    }

    /**
     * @param diagram to set
     */
    public void setDiagram(Diagram diagram) {
        this.diagram = diagram;
    }

}