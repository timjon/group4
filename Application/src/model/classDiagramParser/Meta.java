package model.classDiagramParser;

import java.util.List;

public class Meta {

    private String format;

    private String version;

    private List<Object> extensions = null;

    public List<Object> getExtensions() {
        return extensions;
    }


    private void setExtensions(List<Object> extensions) {
        this.extensions = extensions;
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
