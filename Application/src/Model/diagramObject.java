package Model;

import java.util.List;

public class diagramObject {

    public meta meta;
    String type;
    List<Process> processes;
    //   diagram diagram;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }



    public class meta{

        public String format;
        String version;
        //?? extensions;

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
    //private class diagram{
        //String node;
        //       List<AbstractDocument.Content> content;
 //   }
}