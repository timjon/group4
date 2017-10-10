import javafx.scene.control.Alert;

import java.util.Collection;

public class DiagramCheck {
    public static String diagrams = "sequence_diagram\n";
    public static String ContainsDiagram(Collection<String> s){
        String[] allowedDiagrams = {"sequence_diagram"};

        for(String string: s) {
            for (String diagramType : allowedDiagrams) {
                if (string.contains("\"type\" : \"" + diagramType + "\"")) {

                    disp("Valid Diagram", "The imported diagram has been accepted " +
                            " by the application and is currently being processed by the server.", "Valid diagram");
                    return diagramType;
                }
            }
        }

        System.out.println("\"sType\" : \"" + "sequence_diagram" + "\"");

        disp("Error", "The imported file does not contain one of the supported diagram types, " +
                "the supported diagram types are: \n" + diagrams, "Invalid diagram");
        return "Invalid diagram";
    }
    private static void disp(String frameName, String content, String title) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(frameName);
        alert.setHeaderText(title + " imported");
        alert.setContentText(content);
        alert.show();
    }

}
