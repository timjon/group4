import Model.diagramObject;
import com.google.gson.Gson;




public class Parser {
    public static void Parser(String inputJSON){
        Gson gson = new Gson();
        diagramObject parsedDiagram;

        parsedDiagram = gson.fromJson(inputJSON, Model.diagramObject.class);

        System.out.println("Isabelle is old and blind, needs help to see where output starts");
        System.out.println("Format " + parsedDiagram.meta.getFormat());
        System.out.println("Version " + parsedDiagram.meta.getVersion());
        System.out.println("Type " + parsedDiagram.getType());

        for (diagramObject.Processes element : parsedDiagram.getProcesses() ) {
            System.out.println(element.getClass1());
            System.out.println(element.getName());
        }

//        for (diagramObject.diagram element : parsedDiagram.getDiagram() ) {
//            System.out.println(element.getNode());
//            System.out.println(element.getContent());
//        }

    }
}