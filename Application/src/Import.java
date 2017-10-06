import javafx.scene.control.Alert;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * @version 0.1
 * @author Pontus Laestadius
 * @since 2017-10-06
 */

public class Import {

    /**
     * Opens a Filechooser and enables safe file loading.
     * Not a pure function since it has both side-effects in the form of windows and null values.
     * @param stage the parent stage that the dialog window is displayed on top of.
     * @return a file if the import was successful or a null type otherwise.
     * @author Pontus Laestadius
     */
    public static String file(Stage stage) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Open Resource File");
        File file = fileChooser.showOpenDialog(stage);

        // Verifies the file format
        if (file.toString().endsWith(".json")) {
            StringBuilder content = new StringBuilder();
            try {
                FileReader fr = new FileReader(file);
                BufferedReader br = new BufferedReader(fr);

                String line;
                while ((line=br.readLine()) != null) {
                    content.append(line);
                }

                // Any unexpected exceptions.
            } catch (IOException e) {

                disp(
                        "Unable to read file",
                        "Unexpected exception: \n" + e.toString()
                );

                return null;
            }

            return content.toString();
        }

        // When the verification fails
        disp(
                "Incompatible FileType",
                "The FileType is not supported: \n" + file.toString() +" \n The file must be .json formatted"
        );

        return null;
    }

    /**
     * Displays a dialog box notifying the user that the import action they performed was invalid.
     * @param title of the dialog window.
     * @param content message to be displayed.
     */
    private static void disp(String title, String content) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("Import error: " + title);
        alert.setHeaderText("Import interruption");
        alert.setContentText(content);
        alert.show();
    }
}
