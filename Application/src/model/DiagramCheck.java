package model;

import java.util.Collection;

/**
 * @version 1.0
 * @author Tim Jonasson & Kosara Golemshinska
 * collaborator: Pontus Laestadius
 * @since 2017-10-10
 */

class DiagramCheck {

    // Supported diagram types.
    private final static String[] allowedDiagrams = {"sequence_diagram", "class_diagram", "deployment_diagram"};

    /**
     * Checks if the JSON file contains a diagram type supported by the application.
     * @return diagram type.
     * @param s collection of Strings that make up several JSON file
     */
    static String ContainsDiagram(Collection<String> s){

        for(String string: s) {

            for (String diagramType : allowedDiagrams) {

                // If the collection contains a supported type, return it.
                if (string.contains("\"type\" : \"" + diagramType + "\"")) {
                    return diagramType;
                }
            }
        }

        // Set error message for the dialog box.
        StringBuilder diagrams = new StringBuilder();
        for(String d : allowedDiagrams){

            diagrams.append(d);
            diagrams.append("\n");
        }

        Import.disp(
                "Error",
                "Invalid diagram imported",
                "The imported file does not contain one of the supported diagram types, the supported diagram " +
                        "types are: \n" + diagrams
        );

        return "Invalid diagram";
    }
}