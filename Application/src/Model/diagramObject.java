package Model;

//import javax.swing.text.AbstractDocument;
import java.util.List;

public class diagramObject {


    //handle diagram's meta
    public meta meta;

    //handle the Type of the diagram
    String type;

    public void setType(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

    //handle the Processes of the diagram
    List<Processes> processes;

    public void setProcesses(List<Processes> processes) {
        this.processes = processes;
    }

    public List<Processes> getProcesses() {
        return processes;
    }


    //handle the messages of the diagram
//    List<diagram> diagram;
//    public List<diagramObject.diagram> getDiagram() {
//        return diagram;
//    }
//
//    public void setDiagram(List<diagramObject.diagram> diagram) {
//        this.diagram = diagram;
//    }
//


    public class meta{

        public String format;
        String version;
        //TODO : find out what the purpose is
        List<String> extensions;

        public void setExtensions(List<String> extensions) {
            this.extensions = extensions;
        }

        public List<String> getExtensions() {
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

        String name;
        String class1;


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

//    public class diagram {
//
//        String node;
//        List<Content> content;
//
//        public String getNode() {
//            return node;
//        }
//
//        public void setNode(String node) {
//            this.node = node;
//        }
//
//
//
//        public List<diagramObject.Content> getContent() {
//            return content;
//        }
//
//        public void setContent(List<diagramObject.Content> content) {
//            this.content = content;
//        }
//
//
//    }

//    public class Content {
//
//        String node;
//        String from;
//        String to;
//        List<String> message;
//
//        public String getNode2() {
//            return node;
//        }
//
//        public void setNode2(String node) {
//            this.node = node;
//        }
//
//
//        public String getFrom() {
//            return from;
//        }
//
//        public void setFrom(String from) {
//            this.from = from;
//        }
//
//
//        public String getTo() {
//            return to;
//        }
//
//        public void setTo(String to) {
//            this.to = to;
//        }
//
//
//        public List<String> getMessage() {
//            return message;
//        }
//
//        public void setMessage(List<String> message) {
//            this.message = message;
//        }
//
//
//
//    }


}