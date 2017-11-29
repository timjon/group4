package model;

import controller.Import;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import controller.network.Net;
import view.DiagramView;
import view.ExecutionLog;
import view.handlers.Automate;

import java.util.*;

/**
 * Handles all menu items and their states.
 * @author Pontus Laestadius
 * @version 1.1
 * Collaborator Kosara Golemshinska
 */
public class Menu {

    // Instance
    private static Menu menuInstance;

    // List of buttons.
    private static Button button_import;
    private static Button button_next;
    private static Button button_previous;
    private static Button button_auto;
    private static Button button_class;
    private static Button button_deployment;

    // Flavor texts
    private final static String text_input = "Import";
    private final static String text_auto_play = "|>";
    private final static String text_auto_pause = " || ";
    private final static String text_next = "->";
    private final static String text_previous = "<-";
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
        setMenuState(false, button_import);

        // Adds the event handlers for the buttons.
        setEvents(stage);

        // Add the buttons to the menu.
        menu.getChildren().add(button_import);
        menu.getChildren().add(button_previous);
        menu.getChildren().add(button_auto);
        menu.getChildren().add(button_next);
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
        buttonHashSet.add(button_next);
        buttonHashSet.add(button_previous);
        buttonHashSet.add(button_auto);
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


                        // Enable all media buttons.
                        setMenuState(true);
                        button_previous.setDisable(true);
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

        button_previous.setOnAction((ActionEvent event) ->{
            Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", previous_message}");
        });

        button_next.setOnAction((ActionEvent event)    ->{
            Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", next_message}");
        });

        button_auto.setOnAction((ActionEvent event)    ->{
            play = button_auto.getText().endsWith(text_auto_play);
            identifyState();
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

            // If there is no view, we disable the media buttons.
            Menu.getInstance().setMenuState(false, button_import);
            return;
        }

        // Updates optional view states.
        ArrayList<String> viewing = diagramView.getViewing();

        Platform.runLater(() -> {

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

}
