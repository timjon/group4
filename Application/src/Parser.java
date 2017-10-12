import Model.diagramObject;
import com.google.gson.Gson;

public class Parser {
 public static void Parser(String inputJSON){
     Gson gson = new Gson();

    diagramObject parsedDiagram = gson.fromJson(inputJSON, Model.diagramObject.class);

    System.out.println("Isabelle is old and blind, needs help to see where output starts");
    System.out.println("Format " + parsedDiagram.meta.getFormat());
    System.out.println("Version " + parsedDiagram.meta.getVersion());
    System.out.println("Type " + parsedDiagram.getType());

 }
}