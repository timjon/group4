import javafx.scene.control.Alert;
import java.util.Collection;

/**
 * @version 0.1
 * @author Tim Jonasson & Kosara Golemshinska
 * @since 2017-10-10
 */

public class DiagramCheck {
	// Supported diagram types.
	public static String diagrams = "sequence_diagram\n";
	
	/**
    * Checks if the JSON file contains a diagram type supported by the application.
    * @return diagram type.
    * @param collection of Strings that make up the JSON file
    */
    public static String ContainsDiagram(Collection<String> s){
		// Supported diagram types. Allows for future expansion.
        String[] allowedDiagrams = {"sequence_diagram"};
        for(String string: s) {
            for (String diagramType : allowedDiagrams) {
				// If the collection contains a supported type, return it.
                if (string.contains("\"type\" : \"" + diagramType + "\"")) {
                    return diagramType;
                }
            }
        }
		// Set error message for the dialog box.
        disp("Error", "The imported file does not contain one of the supported diagram types, " +
                "the supported diagram types are: \n" + diagrams, "Invalid diagram");
        return "Invalid diagram";
    }
	
	/**
    * Displays a dialog notifying the user the diagram type is not supported.
    * @param title of the dialog window.
    * @param content message to be displayed.
    */
    private static void disp(String frameName, String content, String title) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(frameName);
        alert.setHeaderText(title + " imported");
        alert.setContentText(content);
        alert.show();
    }

}
