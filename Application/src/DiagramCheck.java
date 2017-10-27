import javafx.scene.control.Alert;
import java.util.Collection;

/**
 * @version 0.1
 * @author Tim Jonasson & Kosara Golemshinska
 * @since 2017-10-10
 */

public class DiagramCheck {
	// Supported diagram types.

    private static String[] allowedDiagrams = {"sequence_diagram"};
	
	/**
    * Checks if the JSON file contains a diagram type supported by the application.
    * @return diagram type.

    * @param s collection of Strings that make up the JSON file
    */
    public static String ContainsDiagram(Collection<String> s){
		// Supported diagram types. Allows for future expansion.
        for(String string: s) {
            for (String diagramType : allowedDiagrams) {
				// If the collection contains a supported type, return it.
                if (string.contains("\"type\" : \"" + diagramType + "\"")) {
                    return diagramType;
                }
            }
        }
		// Set error message for the dialog box.

        disp("Error", "Invalid diagram");
        return "Invalid diagram";
    }
	
	/**
    * Displays a dialog notifying the user the diagram type is not supported.
    * @param frameName The name of the frame shown to the user.
    * @param title of the dialog window.
    */
    private static void disp(String frameName, String title) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(frameName);
        alert.setHeaderText(title + " imported");
        String diagrams = "";
        for(String d : allowedDiagrams){
            diagrams += d + "\n";
        }
        alert.setContentText("The imported file does not contain one of the supported diagram types, " +
                "the supported diagram types are: \n" + diagrams);
        alert.show();
    }

}