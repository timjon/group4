package Model;

import com.google.gson.annotations.SerializedName;
import java.util.List;

/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @since 2017-10-16
 */

public class diagramObject {


    //handle diagram's meta
    private Meta meta;
    //handle the Type of the diagram
    private String type;
    //handle the Processes of the diagram
    private List<Processes> processes = null;
    //handle the messages of the diagram
    private Diagram diagram;


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

        /*The string was named "sequenceDiagramClass" instead of "class" because the word class is protected in java,
        *the @SerializedName annotation will make the Gson library see "class" instead of "sequenceDiagramClass" when matching and parsing.
         */
        @SerializedName("class")
        private String sequenceDiagramClass;

        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getSequenceDiagramClass() {
            return sequenceDiagramClass;
        }

        public void getSequenceDiagramClass(String class1) {
            this.sequenceDiagramClass = class1;
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