import Model.diagramObject;
import com.google.gson.Gson;




public class Parser {
    public static void Parser(String inputJSON){
        Gson gson = new Gson();
        diagramObject parsedDiagram;

        parsedDiagram = gson.fromJson(inputJSON, Model.diagramObject.class);

        System.out.println("\n" + "Meta: \n");
        diagramObject.Meta metaElement = parsedDiagram.getMeta();
        System.out.println(metaElement.getFormat() + "\n" + metaElement.getVersion() + "\n" + parsedDiagram.getType());


        System.out.println("\n" + "Processes: \n");
        for (diagramObject.Processes processesElement : parsedDiagram.getProcesses()) {
            System.out.println( processesElement.getClass1() + " : " + processesElement.getName()+ " \n ");
        }


        System.out.println("\n" + "Diagram Elements: \n");
        for (diagramObject.ContentArray diagramElement : parsedDiagram.getDiagram().getContent().get(0).getContent()) {
            System.out.println(diagramElement.getFrom() + " to " + diagramElement.getTo() + " " + diagramElement.getNode() + "s: [" + diagramElement.getMessage().get(0) + ", " + diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "] \n ");
        }



        System.out.println("\n" + "Parallel Diagram Elements: \n");
        for (diagramObject.ContentArray parallelDiagramElement : parsedDiagram.getDiagram().getContent().get(1).getContent()) {
            System.out.println(parallelDiagramElement.getFrom() + " to " + parallelDiagramElement.getTo() + " " + parallelDiagramElement.getNode() + "s: [" + parallelDiagramElement.getMessage().get(0) + "] \n ");
        }
		
	}
}