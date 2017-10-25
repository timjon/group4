import Model.diagramObject;
import javafx.scene.control.Alert;
import com.google.gson.Gson;


/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @since 2017-10-16
 *
 * made with usage of Gson
 * Gson is released under the Apache 2.0 license.

    Copyright 2008 Google Inc.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
 */

public class Parser {

    /** defining two different counter variables that will increment each time the diagram calling method
    *that they are is called in order to differentiate between the imported diagrams
    */
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
     * Parses the imported JSON files
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */

    public static void parse(String inputJSON) {
        Gson gson = new Gson();

        try {
            diagramObject parsedDiagram;
            parsedDiagram = gson.fromJson(inputJSON, diagramObject.class);

            // get the Meta data
            diagramObject.Meta metaElement = parsedDiagram.getMeta();

            // get the diagram type
            parsedDiagram.getType();

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

        catch (NullPointerException e) {
            syntaxErrorMessage();

        }
    }

    // displays a frame notifying the user that the imported file contains unrecognisable syntax
    private static void syntaxErrorMessage() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Import failed");
        alert.setHeaderText("Unknown syntax");
        alert.setContentText("The selected file contains unrecognisable syntax");
        alert.show();
    }

    /**
     * gives a String containing the first diagram to be handled by the backend
     * @return FirstSequenceDiagram which contains a counter, the processes, the class names, the first diagram's messages and content.
     */

    public static String getFirstSequenceDiagram(){

        String FirstSequenceDiagram = "{" + counter + ",[" + properProcesses + "]," + "["+ properClassNames +"],"+ "["+ properFirstDiagram + "]}";
        // increase the counter by 2 since it is always an even number
        counter = counter + 2;

        return FirstSequenceDiagram;
    }

    /**
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
     * @return ParallelSequenceDiagram which contains a counter, the processes, the class names, the parallel diagram's messages and content.
     */

    public static String getParallelSequenceDiagram(){

        String ParallelSequenceDiagram = "{" + parallelCounter + ",[" + properProcesses + "]," + "["+ properClassNames +"],"+ "["+ properParallelDiagram + "]}";
        // increase the parallelCounter by 2 since it is always an even number
        parallelCounter = parallelCounter + 2;

        return ParallelSequenceDiagram;
    }

}