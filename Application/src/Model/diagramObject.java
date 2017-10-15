package Model;

import com.google.gson.annotations.SerializedName;
import java.util.List;

public class diagramObject {




    //handle diagram's meta
    private Meta meta;

    public Meta getMeta() {
        return meta;
    }

    public void setMeta(Meta meta) {
        this.meta = meta;
    }


    //handle the Type of the diagram
    private String type;

    public void setType(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }


    //handle the Processes of the diagram
    private List<Processes> processes = null;

    public void setProcesses(List<Processes> processes) {
        this.processes = processes;
    }

    public List<Processes> getProcesses() {
        return processes;
    }


    //handle the messages of the diagram
    private Diagram diagram;

    public Diagram getDiagram() {
        return diagram;
    }

    public void setDiagram(Diagram diagram) {
        this.diagram = diagram;
    }



    public class Meta {

        private String format;
        private String version;

        private List<Object> extensions = null;

        private void setExtensions(List<Object> extensions) {
            this.extensions = extensions;
        }

        public List<Object> getExtensions() {
            return extensions;
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

    public class Processes {


        @SerializedName("class")
        private String class1;

        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getClass1() {
            return class1;
        }

        public void setClass1(String class1) {
            this.class1 = class1;
        }




    }

    public class Diagram {


        private String node;

        private List<Content> content = null;

        public String getNode() {
            return node;
        }

        public void setNode(String node) {
            this.node = node;
        }

        public List<Content> getContent() {
            return content;
        }

        public void setContent(List<Content> content) {
            this.content = content;
        }

    }

    public class ContentArray {

        private String node;

        private String from;
        private String to;

        private List<String> message = null;

        public String getNode() {
            return node;
        }

        public void setNode(String node) {
            this.node = node;
        }

        public String getFrom() {
            return from;
        }

        public void setFrom(String from) {
            this.from = from;
        }

        public String getTo() {
            return to;
        }

        public void setTo(String to) {
            this.to = to;
        }

        public List<String> getMessage() {
            return message;
        }

        public void setMessage(List<String> message) {
            this.message = message;
        }

    }

    public class Content {


        private String node;

        private List<ContentArray> content = null;

        public String getNode() {
            return node;
        }

        public void setNode(String node) {
            this.node = node;
        }

        public List<ContentArray> getContent() {
            return content;
        }

        public void setContent(List<ContentArray> content) {
            this.content = content;
        }

    }


}