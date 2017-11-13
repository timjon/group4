package model;

import model.sequenceDiagramParser.Messages;
import model.sequenceDiagramParser.Processes;
import javafx.scene.control.Alert;
import com.google.gson.Gson;
import model.sequenceDiagramParser.SequenceDiagramObject;

import java.util.List;


/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * @version 1.2
 * @since 2017-10-16
 * <p>
 * Made with usage of Gson library for parsing json into Java objects
 * https://github.com/google/gson
 * Gson is released under the Apache 2.0 license.
 * <p>
 * Copyright 2008 Google Inc.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p>
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
     * Parses the imported JSON files if they contain a sequence diagram adheres with the predetermined JSON format
     *
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */

    public void parseSequenceDiagram(String inputJSON) {
        Gson gson = new Gson();

        try {
            SequenceDiagramObject parsedSequenceDiagram;
            parsedSequenceDiagram = gson.fromJson(inputJSON, SequenceDiagramObject.class);

            // Creates an instance of the diagram's meta
            model.sequenceDiagramParser.Meta meta = parsedSequenceDiagram.getMeta();
            // Assigns a String to the diagram's format
            String format = meta.getFormat();
            // Assigns a String to the diagram's extensions
            List<Object> extensions = meta.getExtensions();
            // Assigns a String to the diagram's version
            String version = meta.getVersion();
            // Assigns a String to the type of the diagram
            String type = parsedSequenceDiagram.getType();

            // get the tempProcesses which contain the class names
            for (Processes processesElement : parsedSequenceDiagram.getProcesses()) {
                tempProcesses += "\"" + processesElement.getSequenceDiagramClass() + ":" + processesElement.getName() + "\",";
                processesString = (tempProcesses.substring(0, tempProcesses.length() - 1));
                // yield only the names of the Classes
                tempClassNames += processesElement.getName() + ",";
                //removing extra comma
                classNamesString = (tempClassNames.substring(0, tempClassNames.length() - 1));
            }

            // get the elements of the First Diagram
            for (Messages diagramElement : parsedSequenceDiagram.getDiagram().getContent().get(0).getMessages()) {
                tempFirstDiagram += "{" + diagramElement.getFrom() + "," + diagramElement.getTo() + ",[" + diagramElement.getMessage().get(0) + ", " +
                        diagramElement.getMessage().get(1) + ", " + diagramElement.getMessage().get(2) + "]" + "}" + ",";
                //removing extra comma
                firstDiagramString = (tempFirstDiagram.substring(0, tempFirstDiagram.length() - 1));
            }


            // get the elements of the Parallel Diagram
            for (Messages parallelDiagramElement : parsedSequenceDiagram.getDiagram().getContent().get(1).getMessages()) {
                tempParallelDiagram += "{" + parallelDiagramElement.getFrom() + "," + parallelDiagramElement.getTo() + ",[" + parallelDiagramElement.getMessage().get(0) + "]" + "}" + ",";
                //removing extra comma
                parallelDiagramString = (tempParallelDiagram.substring(0, tempParallelDiagram.length() - 1));
            }

        } catch (Exception e) {
            syntaxErrorMessage();
        }
    }

    /**
     * displays a frame notifying the user that the imported file contains unrecognisable syntax
     */

    private void syntaxErrorMessage() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Import failed");
        alert.setHeaderText("Unknown syntax");
        alert.setContentText("The selected file contains unrecognisable syntax");
        alert.show();
    }

    /**
     * gives a String containing the first diagram to be handled by the backend
     *
     * @return FirstSequenceDiagram which contains a counter, the tempProcesses, the class names, the first diagram's messages and content.
     */

    public String getFirstSequenceDiagram() {

        String FirstSequenceDiagram = "{" + counter + ",[" + processesString + "]," + "[" + classNamesString + "]," + "[" + firstDiagramString + "]}";
        // increase the counter by 2 since it is always an odd number and it started with 1
        counter = counter + 2;

        return FirstSequenceDiagram;
    }

    /**
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
     *
     * @return ParallelSequenceDiagram which contains a counter, the tempProcesses, the class names, the parallel diagram's messages and content.
     */

    public String getParallelSequenceDiagram() {

        String ParallelSequenceDiagram = "{" + parallelCounter + ",[" + processesString + "]," + "[" + classNamesString + "]," + "[" + parallelDiagramString + "]}";
        // increase the parallelCounter by 2 since it is always an even number and it started with 2
        parallelCounter = parallelCounter + 2;

        return ParallelSequenceDiagram;
    }
}