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
 * @version 1.0
 * @since 2017-10-16

 *
 * Made with usage of Gson library for parsing json into Java objects
 * https://github.com/google/gson
 * Gson is released under the Apache 2.0 license.
 *
 * Copyright 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


public class Parser {


    /**
     * defining two different counter variables that will increment each time the diagram calling method
     * that they are is called in order to differentiate between the imported diagrams
     * These two counters are static in order that they change each time and new diagram is imported from the main method
     */

    // odd number for the First Diagram counter
    private static int counter = 1;
    // even number for the parallel Diagram counter
    private static int parallelCounter = 2;

    /**
     * The following declarations are used for concatenating the final returned Strings to be used by the backend
     * The temp declarations store the parsed values but with the extra comma, an extra line in each for each loop removes
     * the extra comma and stores the proper value in a new String variable
     */

    private String tempProcesses = "";
    private String processesString = "";

    private String tempClassNames = "";
    private String classNamesString = "";

    private String tempFirstDiagram = "";
    private String firstDiagramString = "";

    private String tempParallelDiagram = "";
    private String parallelDiagramString = "";


    /**
<<<<<<< HEAD
     * Parses the imported JSON files if they contain a sequence diagram adheres with the predetermined JSON format
     *
=======
     * Parses the imported JSON files and prints it to the console
>>>>>>> a804e35... added comments, integrated with diagramCheck
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */

    public void parseSequenceDiagram(String inputJSON) {
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

            // get the tempProcesses which contain the class names
            for (Processes processesElement : parsedDiagram.getProcesses()) {
                tempProcesses += "'" + processesElement.getSequenceDiagramClass() + ":" + processesElement.getName() + "',";
                processesString = (tempProcesses.substring(0, tempProcesses.length() - 1));
                // yield only the names of the classes
                tempClassNames += processesElement.getName() + ",";
                //removing extra comma
                classNamesString = (tempClassNames.substring(0, tempClassNames.length() - 1));
            }

            // get the elements of the First Diagram
            for (ContentArray diagramElement : parsedDiagram.getDiagram().getContent().get(0).getContent()) {
                tempFirstDiagram += "{" + diagramElement.getFrom() + "," + diagramElement.getTo() + ",[" + diagramElement.getMessage().get(0) + ", " + diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "]" + "}" + ",";
                //removing extra comma
                firstDiagramString = (tempFirstDiagram.substring(0, tempFirstDiagram.length() - 1));
            }


            // get the elements of the Parallel Diagram
            for (ContentArray parallelDiagramElement : parsedDiagram.getDiagram().getContent().get(1).getContent()) {
                tempParallelDiagram += "{" + parallelDiagramElement.getFrom() + "," + parallelDiagramElement.getTo() + ",[" + parallelDiagramElement.getMessage().get(0) + "]" + "}" + ",";
                //removing extra comma
                parallelDiagramString = (tempParallelDiagram.substring(0, tempParallelDiagram.length() - 1));
            }
<<<<<<< HEAD
        }

        System.out.println("\n" + "Properly Formatted String of First Diagram: \n");
        System.out.println(getFirstSequenceDiagram());
        System.out.println("\n" + "Properly Formatted String of Parallel Diagram: \n");
        System.out.println(getParallelSequenceDiagram());
	}

=======
            System.out.println(getFirstSequenceDiagram());
        } catch (NullPointerException e) {
            syntaxErrorMessage();

<<<<<<< HEAD
        }
    }
=======
    /**
     * gives a String containing the first diagram which is formatted in a specific way to be handled by the backend
     * @return FirstSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
     */
>>>>>>> a804e35... added comments, integrated with diagramCheck

    /**
     * displays a frame notifying the user that the imported file contains unrecognisable syntax
     */

<<<<<<< HEAD
    private void syntaxErrorMessage() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Import failed");
        alert.setHeaderText("Unknown syntax");
        alert.setContentText("The selected file contains unrecognisable syntax");
        alert.show();
    }
>>>>>>> 66faf1d... comments, fixes adhering with code review requested changes

    /**
<<<<<<< HEAD
     * gives a String containing the first diagram which is formatted in a specific way to be handled by the backend
     * @return FirstSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
=======
     * gives a String containing the first diagram to be handled by the backend
<<<<<<< HEAD
     * @return FirstSequenceDiagram which contains a counter, the processes, the class names, the first diagram's messages and content.
>>>>>>> a1dd0b6... the parsed string now also returns the class names seperately
=======
     *
     * @return FirstSequenceDiagram which contains a counter, the tempProcesses, the class names, the first diagram's messages and content.
>>>>>>> 66faf1d... comments, fixes adhering with code review requested changes
     */

    public String getFirstSequenceDiagram() {

        String FirstSequenceDiagram = "{" + counter + ",[" + processesString + "]," + "[" + classNamesString + "]," + "[" + firstDiagramString + "]}";
        // increase the counter by 2 since it is always an odd number and it started with 1
=======
        String FirstSequenceDiagram = "{" + counter + ",[" + properProcesses + "]," + "["+ properFirstDiagram + "]}";
        // increase the counter by 2 since it is always an even number
>>>>>>> a804e35... added comments, integrated with diagramCheck
        counter = counter + 2;

        return FirstSequenceDiagram;
    }

    /**
<<<<<<< HEAD
<<<<<<< HEAD
     * gives a String containing the second (i.e parallel) diagram which is formatted in a specific way to be handled by the backend
     * @return ParallelSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
=======
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
<<<<<<< HEAD
     * @return ParallelSequenceDiagram which contains a counter, the processes, the class names, the parallel diagram's messages and content.
>>>>>>> a1dd0b6... the parsed string now also returns the class names seperately
=======
     *
     * @return ParallelSequenceDiagram which contains a counter, the tempProcesses, the class names, the parallel diagram's messages and content.
>>>>>>> 66faf1d... comments, fixes adhering with code review requested changes
     */

    public String getParallelSequenceDiagram() {

        String ParallelSequenceDiagram = "{" + parallelCounter + ",[" + processesString + "]," + "[" + classNamesString + "]," + "[" + parallelDiagramString + "]}";
        // increase the parallelCounter by 2 since it is always an even number and it started with 0
=======
     * gives a String containing the second (i.e parallel) diagram which is formatted in a specific way to be handled by the backend
     * @return ParallelSequenceDiagram which contains a counter, the class names, the first diagram's messages and content.
     */

    public static String getParallelSequenceDiagram(){

        String ParallelSequenceDiagram = "{" + parallelCounter + ",[" + properProcesses + "]," + "["+ ProperParallelDiagram + "]}";
        // increase the parallelCounter by 2 since it is always an even number
>>>>>>> a804e35... added comments, integrated with diagramCheck
        parallelCounter = parallelCounter + 2;

        return ParallelSequenceDiagram;
    }

}