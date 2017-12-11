package controller;

import controller.network.Net;
import javafx.scene.control.Alert;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import model.Parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @version 1.1
 * @author Pontus Laestadius
 * @since 2017-10-06
 */

public class Import {

    /**
     * Opens a FileChooser and enables safe file loading.
     * Not a pure function since it has both side-effects in the form of windows and null values.
     * @param stage the parent stage that the dialog window is displayed on top of.
     * @return a file if the import was successful or a null type otherwise.
     * @author Pontus Laestadius
     */
    public static Collection<String> file(Stage stage) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Open Resource File");
        List<File> files_raw;

        // If the fileChooser is closed without choosing a file.
        if ((files_raw = fileChooser.showOpenMultipleDialog(stage) ) == null) return null;

        Collection<File> files_valid = new ArrayList<>();       // Stores all valid files that will be imported.
        List<File> files_invalid = new ArrayList<>();           // Stores all invalid files that can not be imported.
        List<String> files_content = new ArrayList<>();         // Stores the content of the files.

        // Verifies the file format of all files.
        for (File file: files_raw){

            // If the file is not a .json.
            if (!file.toString().endsWith(".json")) {

                // Removes from the valid diagramViews and moves to the invalid diagramViews.
                files_invalid.add(file);
            } else { // If it is a .json.

                StringBuilder content = new StringBuilder();
                try {
                    FileReader fr = new FileReader(file);
                    BufferedReader br = new BufferedReader(fr);

                    // Append each line of the file to a StringBuilder.
                    String line;
                    while ((line=br.readLine()) != null) {
                        content.append(line);
                    }

                    // Any unexpected exceptions.
                } catch (IOException e) {


                    // Removes from the valid diagramViews and moves to the invalid diagramViews.
                    files_invalid.add(file);

                    continue;
                }

                // Stores the imported data.
                files_valid.add(file);
                files_content.add(content.toString());
            }
        }

        // If there were any invalid files, display them all as one error message to the user.
        if (!files_invalid.isEmpty()) {
            StringBuilder sb = new StringBuilder();

            for (File file: files_invalid) {
                sb.append(file.toString());
                sb.append('\n');
            }
            disp("Import error", "Unable to parse the following file(s):", sb.toString());
        }

        return files_content;
    }

    /**
     * Displays a dialog box notifying the user that the import action they performed was invalid.
     * @param title of the dialog window.
     * @param content message to be displayed.
     */
    public static void disp(String title, String headerText, String content) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(title);
        alert.setHeaderText(headerText);
        alert.setContentText(content);
        alert.show();
    }

    /**
     * Handels importing a diagram
     * @param share True if the diagram is supposed to be shared othervise false
     */
    public static void importDiagram(Stage stage, Boolean share) {
        Collection<String> result = Import.file(stage);
        if (result == null) return;
        if (result.isEmpty()) return;

        // Parse the element if it contains a supported diagram
        Parser parse = new Parser();

        for (String file : result) {
            switch (model.DiagramCheck.ContainsDiagram(result)) {
                case "sequence_diagram":
                    parse.parseSequenceDiagram(file);
                    System.out.println(parse.getDiagram()); //TODO REMOVE
                    // Catches if there are no diagrams.
                    try {
                        if(share) {
                            Net.push("{share, " + parse.getDiagram() + "}");
                        }else{
                            Net.push(parse.getDiagram());
                        }
                    } catch (NullPointerException e) {
                        continue;
                    }

                    // Catches a nullpointer exception if there is no parallel diagram.
                    try {
                        if(share) {
                            Net.push("{share, " + parse.getParallelSequenceDiagram() + "}");
                        }else {
                            Net.push(parse.getParallelSequenceDiagram());
                        }
                    } catch (NullPointerException e) {
                        continue;
                    }

                    break;
                case "class_diagram":
                    parse.parseClassDiagram(file);
                    System.out.println(parse.getDiagram()); // TODO print until backend is implemented.
                    if(share) {
                        Net.push("{share, " + parse.getDiagram() + "}");
                    }else{
                        Net.push(parse.getDiagram());
                    }
                    break;

                case "deployment_diagram":
                    try {
                        parse.parseDeploymentDiagram(file);
                        System.out.println(parse.getDiagram()); //TODO TEST FORMAT
                        if (share) {
                            Net.push("{share, " + parse.getDiagram() + "}");
                        } else {
                            Net.push(parse.getDiagram());
                        }
                    }catch(NullPointerException np) {
                        continue;
                    }
                    break;
            }

        }
        // if the diagram is not included in the switch case, check if the diagram is invalid
        model.DiagramCheck.ContainsDiagram(result);
        model.Menu.getInstance().identifyState();
    }
}
