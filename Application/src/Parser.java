import Model.diagramObject;
import com.google.gson.Gson;

/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @since 2017-10-16
 */

public class Parser {

    /** defining two different counter variables that will increment each time the diagram calling method
    *that they are is called in order to differentiate between the imported diagrams
    *odd number for the First
     *
     *
     *          Diagram counter
    */
    // odd number for the First Diagram counter
    static int counter = 1;
    // even number for the parallel Diagram counter
    static int parallelCounter = 2;

    static String Processes = "";
    static String properProcesses = "";

    static String FirstDiagram = "";
    static String properFirstDiagram = "";

    static String ParallelDiagram= "";
    static String ProperParallelDiagram = "";


    /**
     * Parses the imported JSON files
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */

    public static void parse(String inputJSON){
        Gson gson = new Gson();
        diagramObject parsedDiagram;
        parsedDiagram = gson.fromJson(inputJSON, Model.diagramObject.class);

        System.out.println("\n" + "Meta: \n");
        diagramObject.Meta metaElement = parsedDiagram.getMeta();
        System.out.println(metaElement.getFormat() + "\n" + metaElement.getVersion() + "\n" + parsedDiagram.getType());


        System.out.println("\n" + "Processes: \n");
        for (diagramObject.Processes processesElement : parsedDiagram.getProcesses()) {
            System.out.println(processesElement.getSequenceDiagramClass() + " : " + processesElement.getName() + " \n ");
            Processes += processesElement.getSequenceDiagramClass() + ":" + processesElement.getName() + ",";
            properProcesses = (Processes.substring(0, Processes.length() - 1));
        }

        System.out.println("\n" + "First Diagram Elements: \n");
        for (diagramObject.ContentArray diagramElement : parsedDiagram.getDiagram().getContent().get(0).getContent()) {
            System.out.println(diagramElement.getFrom() + " to " + diagramElement.getTo() + " " + diagramElement.getNode() + "s: [" + diagramElement.getMessage().get(0) + ", " + diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "] \n ");
            FirstDiagram += "{" + diagramElement.getFrom() + "," + diagramElement.getTo() + ",[" + diagramElement.getMessage().get(0) + ", " + diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "]" + "}" + ",";
            properFirstDiagram = (FirstDiagram.substring(0, FirstDiagram.length()-1));
        }



        System.out.println("\n" + "Parallel Diagram Elements: \n");
        for (diagramObject.ContentArray parallelDiagramElement : parsedDiagram.getDiagram().getContent().get(1).getContent()) {
            System.out.println(parallelDiagramElement.getFrom() + " to " + parallelDiagramElement.getTo() + " " + parallelDiagramElement.getNode() + "s: [" + parallelDiagramElement.getMessage().get(0) + "] \n ");
            ParallelDiagram += "{" + parallelDiagramElement.getFrom() + "," + parallelDiagramElement.getTo() + ",[" + parallelDiagramElement.getMessage().get(0) +  "]" + "}" + ",";
            ProperParallelDiagram = (ParallelDiagram.substring(0, ParallelDiagram.length()-1));
        }

        System.out.println("\n" + "Properly Formatted String of First Diagram: \n");
        System.out.println(getFirstSequenceDiagram());
        System.out.println("\n" + "Properly Formatted String of Parallel Diagram: \n");
        System.out.println(getParallelSequenceDiagram());
	}


    /**
     * gives a String containing the first diagram to be handled by the backend
     * @return FirstSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
     */

    public static String getFirstSequenceDiagram(){

        String FirstSequenceDiagram = "{" + counter + ",[" + properProcesses + "]," + "["+ properFirstDiagram + "]}";
        // increase the counter by 2 since it is always an even number
        counter = counter + 2;

        return FirstSequenceDiagram;
    }

    /**
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
     * @return ParallelSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
     */

    public static String getParallelSequenceDiagram(){

        String ParallelSequenceDiagram = "{" + parallelCounter + ",[" + properProcesses + "]," + "["+ ProperParallelDiagram + "]}";
        // increase the parallelCounter by 2 since it is always an even number
        parallelCounter = parallelCounter + 2;

        return ParallelSequenceDiagram;
    }

}