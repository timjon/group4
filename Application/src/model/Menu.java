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
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import controller.network.Net;
import view.DiagramView;
import view.ExecutionLog;
import view.handlers.Automate;

import java.util.*;

import static controller.network.Net.changeMessage;

/**
 * Handles all menu items and their states.
 * @author Pontus Laestadius
 * Collaborators: Sebastian Fransson, Tim Jonasson, Kosara Golemshinska
 * @version 1.2
 */
public class Menu {
    //Boolean values to keep track of when to switch states of buttons. 'host' pertains to creating/removing a lobby.
    private static boolean host = false;
    private static boolean joined_lobby = false;

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
    private final static String text_join_lobby = "Join lobby";
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

        // Box to hold all items.
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
        buttonHashSet.add(button_class);
        buttonHashSet.add(button_deployment);

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
            controller.Import.importDiagram(stage, false);
        });

        button_import_lobby.setOnAction((ActionEvent event) -> {
            controller.Import.importDiagram(stage, true);
        });

        button_previous.setOnAction((ActionEvent event) ->{
            changeMessage("previous_message");
        });

        button_next.setOnAction((ActionEvent event)    ->{
            changeMessage("next_message");
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

            host = false;
            identifyState();
        });

        button_join_lobby.setOnAction((ActionEvent event)    ->{
            //Creates the popup window
            Stage joinLobbyStage = new Stage();
            joinLobbyStage.setTitle("Join a lobby");

            VBox vbox = new VBox();

            //Creates the textfield for writing in the lobby id and sets it to not be auto focused by the cursor
            TextField lobbyID = new TextField("");
            lobbyID.setPromptText("Lobby ID");
            lobbyID.setFocusTraversable(false);

            //Creates the textfield for writing in the password and sets it to not be auto focused by the cursor
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

                        //Stops the users from sending messages to the server without writing in a lobby id or password
                        if (id.equals("") || pwd.equals("")) {
                            label.setText("Lobby ID and password needed");
                        } else {
                            Net.push("{share, {join_lobby, {" + lobbyID.getText() + ", " + password.getText() + "}}}");
                            // Joined lobby
                            joined_lobby = true;
                            joinLobbyStage.close();
                        }
                    });

                });

        //if a user presses the leave button this handler is used.
        //which creates a textbox where a user enters desired lobby to leave
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

            if(joined_lobby){
                button_leave_lobby.setDisable(false);
            }else{
                button_leave_lobby.setDisable(true);
            }


            // Join lobby
            button_join_lobby.setDisable(false);

        });

        DiagramView diagramView;

        try {
            diagramView = DiagramView.getDiagramViewInView();

            // If there is no view.
        } catch (IllegalStateException ex) {
            return;
        }


        Platform.runLater(() -> {

            // Enable other views when there is a diagram in view.
            button_class.setDisable(false);
            button_deployment.setDisable(false);

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
     * Creates a separate window where a user enters their desired password for the lobby.
     */
    public void passwordBox(){
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

        String ok_password_text = "OK";
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
                    host = true;
                    primaryStage.close();
                }
            }
        });
    }

    /**
     * Creates a separate window where a user enters the desired lobby to leave.
     */
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
