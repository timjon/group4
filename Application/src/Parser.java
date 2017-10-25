<<<<<<< HEAD
import Model.diagramObject;
=======
import model.sequenceDiagramParser.Processes;
import model.sequenceDiagramParser.ContentArray;
import javafx.scene.control.Alert;
>>>>>>> 6db1ebc... refactoring, bug fixes, comments
import com.google.gson.Gson;
import model.sequenceDiagramParser.SequenceDiagramObject;

/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @Version 1.0
 * @since 2017-10-16
 */

public class Parser {

    // odd number for the First Diagram counter
    private static int counter = 1;
    // even number for the parallel Diagram counter
    private static int parallelCounter = 2;

    /**
     * static decelerations of String variables along with their proper formatting (to be returned for the backend)
     */
    private static String processes = "";
    private static String properProcesses = "";

    private static String classNames = "";
    private static String properClassNames = "";

    private static String firstDiagram = "";
    private static String properFirstDiagram = "";

    private static String parallelDiagram = "";
    private static String properParallelDiagram = "";


    /**
     * Parses the imported JSON files if they contain a sequence diagram and it is made to adhere with a predetermined JSON format
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */

    public static void parseSequenceDiagram(String inputJSON) {
        Gson gson = new Gson();
        diagramObject parsedDiagram;
        parsedDiagram = gson.fromJson(inputJSON, Model.diagramObject.class);

        try {
            SequenceDiagramObject parsedDiagram;
            parsedDiagram = gson.fromJson(inputJSON, SequenceDiagramObject.class);

            // get the Diagram data
            parsedDiagram.getMeta();

            // get the Diagram type
            parsedDiagram.getType();

            // get the processes which contain the class names
            for (Processes processesElement : parsedDiagram.getProcesses()) {
                processes += "'" + processesElement.getSequenceDiagramClass() + ":" + processesElement.getName() + "',";
                properProcesses = (processes.substring(0, processes.length() - 1));
                // yield only the names of the classes
                classNames += processesElement.getName() + ",";
                //removing extra comma
                properClassNames = (classNames.substring(0, classNames.length() - 1));
            }

            // get the elements of the First Diagram
            for (ContentArray diagramElement : parsedDiagram.getDiagram().getContent().get(0).getContent()) {
                firstDiagram += "{" + diagramElement.getFrom() + "," + diagramElement.getTo() + ",[" + diagramElement.getMessage().get(0) + ", " + diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "]" + "}" + ",";
                //removing extra comma
                properFirstDiagram = (firstDiagram.substring(0, firstDiagram.length() - 1));
            }


            // get the elements of the Parallel Diagram
            for (ContentArray parallelDiagramElement : parsedDiagram.getDiagram().getContent().get(1).getContent()) {
                parallelDiagram += "{" + parallelDiagramElement.getFrom() + "," + parallelDiagramElement.getTo() + ",[" + parallelDiagramElement.getMessage().get(0) + "]" + "}" + ",";
                //removing extra comma
                properParallelDiagram = (parallelDiagram.substring(0, parallelDiagram.length() - 1));
            }
        }

        System.out.println("\n" + "Properly Formatted String of First Diagram: \n");
        System.out.println(getFirstSequenceDiagram());
        System.out.println("\n" + "Properly Formatted String of Parallel Diagram: \n");
        System.out.println(getParallelSequenceDiagram());
	}


    /**
<<<<<<< HEAD
     * gives a String containing the first diagram which is formatted in a specific way to be handled by the backend
     * @return FirstSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
=======
     * gives a String containing the first diagram to be handled by the backend
     * @return FirstSequenceDiagram which contains a counter, the processes, the class names, the first diagram's messages and content.
>>>>>>> a1dd0b6... the parsed string now also returns the class names seperately
     */

    public static String getFirstSequenceDiagram(){

        String FirstSequenceDiagram = "{" + counter + ",[" + properProcesses + "]," + "["+ properClassNames +"],"+ "["+ properFirstDiagram + "]}";
        // increase the counter by 2 since it is always an odd number and it started with 1
        counter = counter + 2;

        return FirstSequenceDiagram;
    }

    /**
<<<<<<< HEAD
     * gives a String containing the second (i.e parallel) diagram which is formatted in a specific way to be handled by the backend
     * @return ParallelSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
=======
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
     * @return ParallelSequenceDiagram which contains a counter, the processes, the class names, the parallel diagram's messages and content.
>>>>>>> a1dd0b6... the parsed string now also returns the class names seperately
     */

    public static String getParallelSequenceDiagram(){

        String ParallelSequenceDiagram = "{" + parallelCounter + ",[" + properProcesses + "]," + "["+ properClassNames +"],"+ "["+ properParallelDiagram + "]}";
        // increase the parallelCounter by 2 since it is always an even number and it started with 0
        parallelCounter = parallelCounter + 2;

        return ParallelSequenceDiagram;
    }

}