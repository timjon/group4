package model;

import controller.Import;
import model.classDiagram.ClassDiagram;
import model.classDiagram.Classes;
import model.classDiagram.FieldTuple;
import model.classDiagram.Relationships;
import model.sequenceDiagramParser.Messages;
import model.sequenceDiagramParser.Processes;
import com.google.gson.Gson;
import model.sequenceDiagramParser.SequenceDiagramObject;
import view.handlers.UniqueCounter;

import java.util.List;


/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * collaborator: Pontus Laestadius
 * @version 1.4
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

    /*
     * defining two different counter variables that will increment each time the diagram calling method
     * that they are is called in order to differentiate between the imported diagrams
     * These two counters are static in order that they change each time and new diagram is imported from the main method
     */

    /*
     * The following declarations are used for concatenating the final returned Strings to be used by the backend
     * The temp declarations store the parsed values but with the extra comma, an extra line in each for each loop removes
     * the extra comma and stores the proper value in a new String variable
     */

    private String processesString = "";
    private String classNamesString = "";

    private String diagram;
    private String parallel;

    /**
     * Parses a class diagram.
     * @param inputJSON to parse
     */
    void parseClassDiagram(String inputJSON) {

        try {

            // Parse
            Gson gson = new Gson();
            ClassDiagram cd = gson.fromJson(inputJSON, ClassDiagram.class);

            // Format
            StringBuilder string = new StringBuilder();

            string.append("{");
            string.append(UniqueCounter.getString());
            string.append(",[");
            for (Classes s: cd.classes) {
                string.append("[");
                string.append(s.name);
                string.append(",");
                for (FieldTuple ft: s.fields) {
                    string.append(ft.name);
                    string.append(":");
                    string.append(ft.type);
                    string.append(",");
                }
                string.replace(string.length()-1, string.length(), "],");
            }
            string.replace(string.length()-1, string.length(), "],[");

            for (Relationships rs: cd.relationships) {
                string.append(rs.format());
                string.append(",");
            }
            string.replace(string.length()-1, string.length(), "]}");

            diagram = string.toString();

        } catch (Exception e) {
            e.printStackTrace();
            Import.disp("Import failed", "Unknown syntax", e.toString());
        }
    }

    /**
     * Parses the imported JSON files if they contain a sequence diagram adheres with the predetermined JSON format
     *
     * @param inputJSON the imported collection of Strings that make up the JSON files
     */
    public void parseSequenceDiagram(String inputJSON) {
        String tempProcesses = "";
        String tempClassNames = "";

        Gson gson = new Gson();

        try {
            SequenceDiagramObject parsedSequenceDiagram;
            parsedSequenceDiagram = gson.fromJson(inputJSON, SequenceDiagramObject.class);

            // get the tempProcesses which contain the class names
            for (Processes processesElement : parsedSequenceDiagram.getProcesses()) {
                tempProcesses += "\"" + processesElement.getSequenceDiagramClass() + ":" + processesElement.getName() + "\",";
                processesString = (tempProcesses.substring(0, tempProcesses.length() - 1));
                // yield only the names of the Classes
                tempClassNames += processesElement.getName() + ",";
                //removing extra comma
                classNamesString = (tempClassNames.substring(0, tempClassNames.length() - 1));
            }

            diagram = parseMessages(parsedSequenceDiagram.getDiagram().getContent().get(0).getMessages());
            diagram = wrap(diagram);

            // If there is a parallel diagram.
            if (parsedSequenceDiagram.getDiagram().getContent().size() > 1) {
                parallel = parseMessages(parsedSequenceDiagram.getDiagram().getContent().get(1).getMessages());
                parallel = wrap(parallel);
            }

        } catch (Exception e) {
            e.printStackTrace();
            Import.disp("Import failed", "Unknown syntax", e.toString());
        }
    }

    /**
     * @param string to wrap
     * @return the provided string wrapped inside the processes and class names, in a formatted string.
     */
    private String wrap(String string) {
        return "{" + UniqueCounter.getString() + ",[" + processesString + "]," + "[" + classNamesString + "]," + "[" + string + "]}";
    }

    /**
     * Given a list of Messages (I know, confusing right?) will parse them in to concat string.
     * @param messages list of Messages
     * @return a formatted string.
     */
    private String parseMessages(List<Messages> messages) {
        StringBuilder result = new StringBuilder();
        for (Messages m: messages) {
            String r = "{" + m.getFrom() + "," + m.getTo() + ",[";
            result.append(r);
            for (String s: m.getMessage()) {
                result.append(s);
                result.append(",");
            }
            result.replace(result.length()-1, result.length(), "");
            result.append("]},");
        }
        return result.substring(0, result.length()-1);
    }

    /**
     * gives a String containing the first diagram to be handled by the backend
     *
     * @return a formatted string with the following data,
     * unique counter, the tempProcesses, the class names, the first diagram's messages and content.
     */
    String getDiagram() {
        return diagram;
    }

    /**
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
     *
     * @return contains a counter, the tempProcesses, the class names, the parallel diagram's messages and content.
     */
    String getParallelSequenceDiagram() {
        return parallel;
    }
}