package model;

import model.classDiagram.*;
import model.deploymentDiagram.*;
import model.sequenceDiagramParser.*;

import view.DiagramView;

import controller.Import;

import com.google.gson.Gson;

import java.util.List;


/**
 * @author Rashad Kamsheh & Isabelle Törnqvist
 * collaborator: Pontus Laestadius, Sebastian Fransson
 * @version 2.0
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

    //The following declarations are used for concatenating the final returned Strings to be used by the backend
    private String processesString = "";
    private String classNamesString = "";

    // Stores a general purpose diagram variable.
    private String diagram = null;

    // Stores a possible parallel version of a diagram.
    private String parallel = null;

    /**
     * Parses a deployment diagram.
     * @param inputJSON
     */
    public void parseDeploymentDiagram(String inputJSON){

        try {
            //Parsing the diagram
            Gson gson = new Gson();
            DeploymentDiagram dd = gson.fromJson(inputJSON, DeploymentDiagram.class);

            //Formating the parsed diagram
            StringBuilder deployString = new StringBuilder();
             deployString.append("deployment_diagram,");
             deployString.append("{");
             deployString.append(UniqueCounter.getString()); // adds a unique id to the parsed string.
             deployString.append(",[");

            //Add Mappings to the string.
            for (Mapping maps : dd.mapping) {
                deployString.append("['");
                deployString.append(maps.process);
                deployString.append("','");
                deployString.append(maps.device);
                deployString.append("'],");
            }

            //Remove unnecessary comma and add an end.
            deployString.replace(deployString.length()-1, deployString.length(), "]}");

            diagram = deployString.toString();

        }catch(Exception e){
            e.printStackTrace();
            Import.disp("Import Failed","Unknown Syntax",e.toString());
        }

    }


    /**
     * Parses a class diagram.
     * @param inputJSON to parse
     */
    public void parseClassDiagram(String inputJSON) throws IllegalStateException {

        try {

            // Parse
            Gson gson = new Gson();
            ClassDiagram cd = gson.fromJson(inputJSON, ClassDiagram.class);

            // Format
            StringBuilder string = new StringBuilder();

            string.append("{");
            string.append("class_diagram,");
            string.append(DiagramView.getDiagramViewInView().getTab().getId());
            string.append(",");
            string.append(UniqueCounter.getString());
            string.append(",[");

            // Add the classes.
            for (Classes s: cd.classes) {
                string.append("['");
                string.append(s.name);
                string.append("',");

                // Add the fields for the classes.
                for (FieldTuple ft: s.fields) {
                    string.append("'");
                    string.append(ft.name);
                    string.append(":");
                    string.append(ft.type);
                    string.append("'");
                    string.append(",");
                }

                // Remove extra comma, and apply formatting.
                string.replace(string.length()-1, string.length(), "],");
            }

            // Remove extra comma, and apply formatting.
            string.replace(string.length()-1, string.length(), "],[");

            // Iterate over the relationships and format them.
            for (Relationships rs: cd.relationships) {
                string.append(rs.format());
                string.append(",");
            }

            // Remove extra comma, and apply formatting.
            string.replace(string.length()-1, string.length(), "]}");

            // Set the diagram to be the string version of the formatted class diagram.
            diagram = string.toString();

            // Catch everything and prompt the user with a proper error notification.
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

                tempProcesses += "\"" +
                        processesElement.getSequenceDiagramClass() + ":" +
                        processesElement.getName() + "\",";

                processesString = (tempProcesses.substring(0, tempProcesses.length() - 1));
                // yield only the names of the Classes
                tempClassNames += processesElement.getName() + ",";
                //removing extra comma
                classNamesString = (tempClassNames.substring(0, tempClassNames.length() - 1));
            }

            // Retrieves the parsed messages.
            diagram = parseMessages(parsedSequenceDiagram.getDiagram().getContent().get(0).getMessages());

            // Wraps them up and gives them an unique id.
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
     * Given a list of Messages (plural Messages class) will parse them in to concat string.
     * @param messages list of Messages
     * @return a formatted string.
     */
    private String parseMessages(List<Messages> messages) {
        StringBuilder result = new StringBuilder();

        // Iterate over the available messages and append them to the StringBuilder,
        // But with a specific format.
        for (Messages m: messages) {

            // Add a formatted string for the from and to of the message.
            String r = "{" + m.getFrom() + "," + m.getTo() + ",[";
            result.append(r);

            // Iterate through the message content.
            for (String s: m.getMessage()) {

                result.append(s);
                result.append(",");
            }

            // Removes extra comma, and applies formatting.
            result.replace(result.length()-1, result.length(), "]},");
        }

        // Removes extra comma.
        return result.substring(0, result.length()-1);
    }

    /**
     * gives a String containing the first diagram to be handled by the backend
     * @return a formatted string with the following data,
     * unique counter, the tempProcesses, the class names, the first diagram's messages and content.
     */
    public String getDiagram() throws NullPointerException {
        if (diagram == null) throw new NullPointerException("No diagram parsed");
        return diagram;
    }

    /**
     * gives a String containing the second (i.e parallel) diagram to be handled by the backend
     * @return contains a counter, the tempProcesses, the class names, the parallel diagram's messages and content.
     */
    public String getParallelSequenceDiagram() throws NullPointerException {
        if (diagram == null) throw new NullPointerException("No diagram parsed");

        return parallel;
    }
}
