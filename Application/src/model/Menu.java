package model;

import controller.Import;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import controller.network.Net;
import view.DiagramView;
import view.ExecutionLog;
import view.handlers.Automate;

import java.util.*;

/**
 * Handles all menu items and their states.
 * @author Pontus Laestadius
 * @version 1.0
 */
public class Menu {

    private static boolean host = false;

    // Instance
    private static Menu menuInstance;

    // List of buttons.
    private static Button button_import;
    private static Button button_next;
    private static Button button_previous;
    private static Button button_auto;
    private static Button button_join_lobby;
    private static Button button_create_lobby;
    private static Button button_remove_lobby;
    private static Button button_import_lobby;
    private static Button button_leave_lobby;
    private static Button button_class;
    private static Button button_deployment;

    // Flavor texts
    private final static String text_input = "Import";
    private final static String text_auto_play = "|>";
    private final static String text_auto_pause = " || ";
    private final static String text_next = "->";
    private final static String text_previous = "<-";
    private final static String text_import_lobby = "Import to lobby";
    private final static String text_join_lobby = "Join a lobby";
    private final static String text_create_lobby = "Create lobby";
    private final static String text_remove_lobby = "Remove lobby";
    private final static String text_leave_lobby = "Leave lobby";
    private final static String text_class = "Show class diagram";
    private final static String text_deployment = "Show deployment diagram";

    // State
    private boolean play = false;

    /**
     * Public constructor.
     */
    public Menu() {
        menuInstance = this;
        // Declare the buttons and their text.
        button_import = new Button(text_input);
        button_next = new Button(text_next);
        button_previous = new Button(text_previous);
        button_auto = new Button(text_auto_play);
        button_import_lobby  = new Button(text_import_lobby);
        button_join_lobby = new Button(text_join_lobby);
        button_create_lobby = new Button(text_create_lobby);
        button_remove_lobby = new Button(text_remove_lobby);
        button_leave_lobby = new Button(text_leave_lobby);
        button_class = new Button(text_class);
        button_deployment = new Button(text_deployment);
    }

    /**
     * @param play set the play status of automation.
     */
    private void setPlay(boolean play) {
        this.play = play;
    }

    /**
     * @return a menu instance.
     */
    public static Menu getInstance() {
        return menuInstance;
    }

    /**
     * @param stage stage to use as the main window.
     * @return a menu with all the available buttons.
     */
    public HBox get(Stage stage) {

        // Box to hold all item.
        HBox menu = new HBox();

        // Start all media buttons disabled.
        setMenuState(false, button_import, button_import_lobby, button_join_lobby, button_create_lobby);

        // Adds the event handlers for the buttons.
        setEvents(stage);

        // Add the buttons to the menu.
        menu.getChildren().add(button_import);
        menu.getChildren().add(button_import_lobby);
        menu.getChildren().add(button_previous);
        menu.getChildren().add(button_auto);
        menu.getChildren().add(button_next);
        menu.getChildren().add(button_join_lobby);
        menu.getChildren().add(button_create_lobby);
        menu.getChildren().add(button_remove_lobby);
        menu.getChildren().add(button_leave_lobby);
        menu.getChildren().add(button_class);
        menu.getChildren().add(button_deployment);

        return menu;
    }

    /**
     * Given a disabled state will apply it to all the buttons excluding the exceptions.
     * @param state true to enable, false to disable.
     * @param buttons ignore to modify these.
     */
    private void setMenuState(boolean state, Button... buttons) {
        // Store all the existing buttons one set.
        Set<Button> buttonHashSet = new HashSet<>();

        buttonHashSet.add(button_import);
        buttonHashSet.add(button_import_lobby);
        buttonHashSet.add(button_next);
        buttonHashSet.add(button_previous);
        buttonHashSet.add(button_auto);
        buttonHashSet.add(button_join_lobby);
        buttonHashSet.add(button_create_lobby);
        buttonHashSet.add(button_remove_lobby);
        buttonHashSet.add(button_leave_lobby);

        // Remove the buttons from the set, to not modify.
        for (Button button: buttons)
            buttonHashSet.remove(button);

        // Set the state of the remaining buttons.
        for (Button button: buttonHashSet)
            button.setDisable(!state);
    }

    /**
     * Starts automating the executing. Disables manual control.
     */
    private static void play() {
        // If it is finished, we pause.
        if (ExecutionLog.getInstance().isFinished()) {
            // Pause the execution.
            pause();

            // Identify new states.
            Menu.getInstance().identifyState();
        }

        // Start automating.
        Platform.runLater(() -> {

            button_auto.setText(text_auto_pause);

            // Disable manual controls.
            button_next.setDisable(true);
            button_previous.setDisable(true);

            // Start a new thread if there is no process running.
            if (!Automate.running())
                (new Thread(new Automate())).start();
        });
    }

    /**
     * Stops automating the executing. enables manual control.
     */
    public static void pause() {
        Menu.getInstance().setPlay(false);
        Platform.runLater(() -> {
            button_auto.setText(text_auto_play);

            // Enable manual controls.
            button_next.setDisable(false);
            button_previous.setDisable(false);
        });

        Automate.cancel();
    }

    /**
     * Sets the event handlers for all buttons.
     */
    private void setEvents(Stage stage) {

        button_import.setOnAction((ActionEvent event) -> {
            Collection<String> result = Import.file(stage);
            if (result == null) return;
            if (result.isEmpty()) return;

            // Parse the element if it contains a supported diagram
            Parser parse = new Parser();

            for (String file: result) {
                switch(DiagramCheck.ContainsDiagram(result)) {
                    case "sequence_diagram" :
                        parse.parseSequenceDiagram(file);

                        // Catches if there are no diagrams.
                        try {
                            Net.push(parse.getDiagram());
                        } catch (NullPointerException e) {
                            continue;
                        }

                        // Catches if there is no parallel diagram.
                        try {
                            Net.push(parse.getParallelSequenceDiagram());
                        } catch (NullPointerException e) {
                            continue;
                        }

                        break;
                    case "class_diagram":
                        parse.parseClassDiagram(file);
                        System.out.println(parse.getDiagram()); // TODO print until backend is implemented.
                        Net.push(parse.getDiagram());

                        break;
                }

            }

            identifyState();
        });

        button_import_lobby.setOnAction((ActionEvent event) -> {
            Collection<String> result = Import.file(stage);
            if (result == null) return;
            if (result.isEmpty()) return;

            // Parse the element if it contains a supported diagram
            Parser parse = new Parser();
            switch(DiagramCheck.ContainsDiagram(result)) {
                case "sequence_diagram" :
                    for (String element : result) {
                        parse.parseSequenceDiagram(element);
                        Net.push("{share, " + parse.getDiagram() + "}");
                        System.out.println("{share, " + parse.getDiagram() + "}");

                        // Enable all media buttons.
                    }
                    break;
            }
            for (String file: result) {
                switch(DiagramCheck.ContainsDiagram(result)) {
                    case "sequence_diagram" :
                        parse.parseSequenceDiagram(file);

                        // Catches if there are no diagrams.
                        try {
                            Net.push("{share, " + parse.getDiagram() + "}");
                            System.out.println("{share, " + parse.getDiagram() + "}");
                        } catch (NullPointerException e) {
                            continue;
                        }

                        // Catches if there is no parallel diagram.
                        try {
                            Net.push("{share, " + parse.getParallelSequenceDiagram() + "}");
                        } catch (NullPointerException e) {
                            continue;
                        }

                        break;
                    case "class_diagram":
                        parse.parseClassDiagram(file);
                        System.out.println(parse.getDiagram()); // TODO print until backend is implemented.
                        Net.push("{share, " + parse.getDiagram() + "}");

                        break;
                }

            }
            // if the diagram is not included in the switch case, check if the diagram is invalid
            DiagramCheck.ContainsDiagram(result);
            identifyState();
        });

        button_previous.setOnAction((ActionEvent event) ->{
            String tabID = DiagramView.getDiagramViewInView().getTab().getId();
            try {
                //Checks if the current tab has a diagram from a lobby or not
                if (tabID.charAt(1) == 'l') {
                    Net.push("{share, {" + tabID + ", previous_message}}");
                } else {
                    Net.push("{" + tabID + ", previous_message}");
                }
            }catch(StringIndexOutOfBoundsException e){
                Net.push("{" + tabID + ", previous_message}");
            }
        });

        button_next.setOnAction((ActionEvent event)    ->{
            String tabID = DiagramView.getDiagramViewInView().getTab().getId();
            try {
                //Checks if the current tab has a diagram from a lobby or not
                if (tabID.charAt(1) == 'l') {
                    Net.push("{share, {" + tabID + ", next_message}}");
                } else {
                    Net.push("{" + tabID + ", next_message}");
                }
            }catch(StringIndexOutOfBoundsException e){
                Net.push("{" + tabID + ", next_message}");
            }
        });

        button_auto.setOnAction((ActionEvent event)    ->{
            play = button_auto.getText().endsWith(text_auto_play);
            identifyState();
        });

        button_create_lobby.setOnAction((ActionEvent event)    ->{
            passwordBox();
            identifyState();
        });
        button_remove_lobby.setOnAction((ActionEvent event) ->{
            Net.push("{" + "share, " + "remove_lobby}");

            Platform.runLater(() -> {
                DiagramView.getDiagramViewInView().getTab().setId("Disconnected");
                DiagramView.getDiagramViewInView().getTab().setText("Disconnected");
            });

            host = false;
            identifyState();
        });

        button_join_lobby.setOnAction((ActionEvent event)    ->{
            //Creates the popup window
            Stage joinLobbyStage = new Stage();
            joinLobbyStage.setTitle("Join a lobby");


                    VBox vbox = new VBox();

                    TextField lobbyID = new TextField("");
                    lobbyID.setPromptText("Lobby ID");
                    lobbyID.setFocusTraversable(false);

                    PasswordField password = new PasswordField();
                    password.setPromptText("Password");
                    password.setFocusTraversable(false);

                    Label label = new Label();

            //Creates the join button
            Button join_button = new Button("Join");
            join_button.setFocusTraversable(false);

            //Adds the fields and the button to the window
            vbox.getChildren().add(lobbyID);
            vbox.getChildren().add(password);
            vbox.getChildren().add(join_button);
            vbox.getChildren().add((label));

            //Sets the size of the window
            Scene stageScene = new Scene(vbox, 300, 100);
            joinLobbyStage.setScene(stageScene);
            joinLobbyStage.show();


                    join_button.setOnAction((ActionEvent e) -> {
                        String id = lobbyID.getText();
                        String pwd = password.getText();

                        if (id.equals("") || pwd.equals("")) {
                            label.setText("Lobby ID and password needed");
                        } else {
                            Net.push("{share, {join_lobby, {" + lobbyID.getText() + ", " + password.getText() + "}}}");
                            joinLobbyStage.close();
                        }
                    });

                });
        button_leave_lobby.setOnAction((ActionEvent event) ->{
            leaveLobbyBox();
        });

        button_class.setOnAction((ActionEvent event)    ->{
            DiagramView dv = DiagramView.getDiagramViewInView();

            // Checks for the state of the button if it's hiding or showing.
            if (button_class.getText().toLowerCase().contains("show")) {

                // Change to 'hide'
                button_class.setText("Hide class diagram");

                // Add diagram to view.
                dv.addDiagram(dv.CLASS_DIAGRAM);
            } else {

                // Change to default text.
                button_class.setText(text_class);

                // Remove diagram from view.
                dv.removeDiagram(dv.CLASS_DIAGRAM);
            }

        });

        button_deployment.setOnAction((ActionEvent event)    ->{
            DiagramView dv = DiagramView.getDiagramViewInView();

            // Checks for the state of the button if it's hiding or showing.
            if (button_deployment.getText().toLowerCase().contains("show")) {

                // Change to 'hide'
                button_deployment.setText("Hide deployment diagram");

                // Add diagram to view.
                dv.addDiagram(dv.DEPLOYMENT_DIAGRAM);
            } else {

                // Change to default text.
                button_deployment.setText(text_deployment);

                // Remove diagram from view.
                dv.removeDiagram(dv.DEPLOYMENT_DIAGRAM);
            }
        });

    }

    /**
     * Reads the state of the other objects and sets the buttons appropriately.
     */
    public void identifyState() {

        // If it is currently automating.
        if (play) {
            play();
            return;
        } else {
            pause();
        }

        DiagramView diagramView;

        try {
            diagramView = DiagramView.getDiagramViewInView();

            // If there is no view.
        } catch (IllegalStateException ex) {
            return;
        }

        String id = diagramView.getTab().getId();
        boolean in_lobby = id.contains("l");

        Platform.runLater(() -> {

            if (host) {
                // Remove lobby
                button_remove_lobby.setDisable(false);

                // Create lobby
                button_create_lobby.setDisable(true);

                // Import lobby
                button_import_lobby.setDisable(false);


            } else {
                button_remove_lobby.setDisable(true);
                button_create_lobby.setDisable(false);
                button_import_lobby.setDisable(true);
            }

            // Join lobby
            button_join_lobby.setDisable(false);
            button_leave_lobby.setDisable(false);

        // Updates optional view states.
        ArrayList<String> viewing = diagramView.getViewing();

            // Only displaying required diagram.
            button_class.setText(text_class);
            button_deployment.setText(text_deployment);

            if (viewing.contains("CLASS_DIAGRAM"))
                button_class.setText("Hide class diagram");

            if (viewing.contains("DEPLOYMENT_DIAGRAM"))
                button_deployment.setText("Hide deployment diagram");

            // If we can go back.
            if (diagramView.getDraw().canRemoveMessage()) {
                button_previous.setDisable(false);

                // If we can't go back.
            } else {
                button_previous.setDisable(true);
            }

            // If it is finished.
            if (ExecutionLog.getInstance().isFinished()) {
                button_next.setDisable(true);
                button_auto.setDisable(true);

                // If it's not finished.
            } else {
                button_next.setDisable(false);
                button_auto.setDisable(false);
            }
        });
    }

    /**
     * Method for adding a password to the creation of a lobby.
     */
    public void passwordBox(){
        String ok_password_text = "OK";
        Stage primaryStage = new Stage(); // Creates a new 'stage' where we gather the fields and buttons for the pwBox.
        primaryStage.setTitle("Input Password");
        primaryStage.show();

        GridPane grid = new GridPane(); // Creates a gridpane to easily place the different components.
        grid.setAlignment(Pos.CENTER);
        //Sets a "padding" which makes the texfield and related components more centered and not use the entire box.
        grid.setHgap(10);
        grid.setVgap(10);
        grid.setPadding(new Insets(15, 15, 15, 15));

        Scene scene = new Scene(grid, 250, 125);
        primaryStage.setScene(scene);

        Label pw = new Label("Password:"); // Label showing showing next to textField.
        grid.add(pw, 0, 0);

        PasswordField pwBox = new PasswordField(); // Textfield with password protection.
        grid.add(pwBox, 1, 0);
        pwBox.setPromptText("Password");
        Button button_ok_password = new Button(ok_password_text); // Button to create a lobby with input password.
        grid.add(button_ok_password, 1,1);

        final Text pressReturn = new Text(); //
        grid.add(pressReturn, 1, 2);

        //Handler for pressing the ok button in the newly opened window.
        button_ok_password.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                final String result = pwBox.getText();
                if(result.equals("")) pressReturn.setText("Please enter a password");
                else {
                    Net.push("{" + "share, " + result + ", create_lobby}");
                    pressReturn.setFill(Color.GREEN);
                    pressReturn.setText("Lobby created");
                    host = true;
                    primaryStage.close();
                }
            }
        });
    }

    public void leaveLobbyBox(){
        String ok_leave_text = "OK";
        Stage leaveStage = new Stage(); // Creates a new 'stage' where we gather the fields and buttons for the box.
        leaveStage.setTitle("Input Lobby ID");
        leaveStage.show();

        GridPane grid = new GridPane(); // Creates a GridPane to easily place the different components.
        grid.setAlignment(Pos.CENTER);
        //Sets a "padding" which makes the TexField and related components more centered and not use the entire box.
        grid.setHgap(10);
        grid.setVgap(10);
        grid.setPadding(new Insets(15, 15, 15, 15));

        Scene scene = new Scene(grid, 250, 125);
        leaveStage.setScene(scene);
        TextField leaveBox = new TextField();
        grid.add(leaveBox, 1, 0);
        leaveBox.setPromptText("Lobby ID");
        Button button_ok_leave = new Button(ok_leave_text); // Button to leave a lobby with id.
        grid.add(button_ok_leave, 1,1);

        final Text pressReturn = new Text(); //
        grid.add(pressReturn, 1, 2);

        //Handler for pressing the ok button in the newly opened window.
        button_ok_leave.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                final String result = leaveBox.getText();
                    // Send the result to the server.
                    Net.push("{" + "share, " + result + ", leave_lobby}");
                    leaveStage.close();
                }
        });
    }

}
