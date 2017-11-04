package model.classDiagramParser;

import java.util.List;

public class ClassDiagramObject {


    private Meta meta;

    private String type;

    private List<Classes> classes = null;




    public Meta getMeta() {
        return meta;
    }

    public String getType() {
        return type;
    }

    public List<Classes> getClasses() {
        return classes;
    }
}
