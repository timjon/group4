package model.sequenceDiagramParser;

import java.util.List;

/**
 * This Class is part Sequence Diagram Object for the parser from JSON to POJo (Plain Old Java Objects)
 * The getters and setters and their decelerations are made to adhere with a predetermined JSON format
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @Version 1.0
 * @since 2017-10-16
 */

public class SequenceDiagramObject {


    //handle diagram's meta
    private Meta meta;
    //handle the Type of the diagram
    private String type;
    //handle the Processes of the diagram
    private List<Processes> processes = null;
    //handle the messages of the diagram
    private Diagram diagram;



    // getters and setters

    public Meta getMeta() {
        return meta;
    }

    public void setMeta(Meta meta) {
        this.meta = meta;
    }


    public void setType(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }


    public void setProcesses(List<Processes> processes) {
        this.processes = processes;
    }

    public List<Processes> getProcesses() {
        return processes;
    }


    public Diagram getDiagram() {
        return diagram;
    }

    public void setDiagram(Diagram diagram) {
        this.diagram = diagram;
    }


}