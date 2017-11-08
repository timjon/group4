package model;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import net.Net;
import visuals.DiagramView;
import visuals.ExecutionLog;
import visuals.handlers.Automate;

import java.util.*;

/**
 * @author Pontus Laestadius
 * @version 1.0
 */
public class Menu {

    // Instance
    private static Menu menuInstance;

    // List of buttons.
    private static Button button_import;
    private static Button button_next;
    private static Button button_previous;
    private static Button button_auto;

    // Flavor texts
    private final static String text_input = "Import";
    private final static String text_auto_play = "|>";
    private final static String text_auto_pause = " || ";
    private final static String text_next = "->";
    private final static String text_previous = "<-";

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
        setMenuState(false, button_import);

        // Adds the event handlers for the buttons.
        setEvents(stage);

        // Add the buttons to the menu.
        menu.getChildren().add(button_import);
        menu.getChildren().add(button_previous);
        menu.getChildren().add(button_auto);
        menu.getChildren().add(button_next);

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
        Platform.runLater(() -> {
            button_auto.setText(text_auto_pause);

            // Disable manual controls.
            button_next.setDisable(true);
            button_previous.setDisable(true);

            (new Thread(new Automate())).start();
        });
    }

    /**
     * Stops automating the executing. enables manual control.
     */
    public static void pause() {
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

                // Parse the element if it contains a supported diagram
                Parser parse = new Parser();
                switch(DiagramCheck.ContainsDiagram(result)) {
                    case "sequence_diagram" :
                        for (String element : result) {
                            parse.parseSequenceDiagram(element);
                            Net.push(parse.getFirstSequenceDiagram());

                            // Enable all media buttons.
                            setMenuState(true);
                            button_previous.setDisable(true);
                        }
                        break;
                }
                // if the diagram is not included in the switch case, check if the diagram is invalid
                DiagramCheck.ContainsDiagram(result);
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

        // Is it at the beginning of execution.

        DiagramView diagramView;

        try {
            diagramView = DiagramView.getDiagramViewInView();

            // If there is no view.
        } catch (IllegalStateException ex) {

            // If there is no view, we disable the media buttons.
            Menu.getInstance().setMenuState(false, button_import);
            return;
        }

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

    }

}