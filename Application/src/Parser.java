import Model.diagramObject;
import com.google.gson.Gson;

/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @since 2017-10-16
 */

public class Parser {

    // odd number for the First Diagram counter
    static int counter = 1;
    // even number for the parallel Diagram counter
    static int parallelCounter = 2;

    static String processes = "";
    static String properProcesses = "";

    static String classNames = "";
    static String properClassNames = "";

    static String firstDiagram = "";
    static String properFirstDiagram = "";

    static String parallelDiagram = "";
    static String properParallelDiagram = "";


    /**
     * Parses the imported JSON files and prints it to the console
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */

    public static void Parser(String inputJSON){
        Gson gson = new Gson();
        diagramObject parsedDiagram;
        parsedDiagram = gson.fromJson(inputJSON, Model.diagramObject.class);

        try {
            diagramObject parsedDiagram;
            parsedDiagram = gson.fromJson(inputJSON, diagramObject.class);

            // get the Meta data
            diagramObject.Meta metaElement = parsedDiagram.getMeta();

            // get the processes which contains the class names
            for (diagramObject.Processes processesElement : parsedDiagram.getProcesses()) {
                processes += "'" + processesElement.getSequenceDiagramClass() + ":" + processesElement.getName() + "',";
                properProcesses = (processes.substring(0, processes.length() - 1));
                // yield only the names of the classes
                classNames += processesElement.getName() + ",";
                properClassNames = (classNames.substring(0, classNames.length() - 1));
            }

            // get the elements of the First Diagram
            for (diagramObject.ContentArray diagramElement : parsedDiagram.getDiagram().getContent().get(0).getContent()) {
                firstDiagram += "{" + diagramElement.getFrom() + "," + diagramElement.getTo() + ",[" + diagramElement.getMessage().get(0) + ", " + diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "]" + "}" + ",";
                properFirstDiagram = (firstDiagram.substring(0, firstDiagram.length() - 1));
            }


            // get the elements of the Parallel Diagram
            for (diagramObject.ContentArray parallelDiagramElement : parsedDiagram.getDiagram().getContent().get(1).getContent()) {
                parallelDiagram += "{" + parallelDiagramElement.getFrom() + "," + parallelDiagramElement.getTo() + ",[" + parallelDiagramElement.getMessage().get(0) + "]" + "}" + ",";
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
        // increase the counter by 2 since it is always an even number
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
        // increase the parallelCounter by 2 since it is always an even number
        parallelCounter = parallelCounter + 2;

        return ParallelSequenceDiagram;
    }

}