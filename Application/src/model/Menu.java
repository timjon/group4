package model;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import net.Net;
import visuals.DiagramView;
import visuals.ExecutionLog;
import visuals.handlers.Automate;

import java.util.Collection;

public class Menu {
    static Button button_import;
    static Button button_next;
    static Button button_previous;
    static Button button_auto;

    // Flavor texts
    private final static String text_auto_play = "|>";
    private final static String text_auto_pause = " || ";
    private final static String text_next = "->";
    private final static String text_previous = "<-";


    public static HBox get(Stage stage) {
        HBox menu = new HBox();

        // Declare the buttons and their text.
        button_import = new Button("Import");
        button_next = new Button(text_next);
        button_previous = new Button(text_previous);
        button_auto = new Button(text_auto_play);

        // Start all media buttons disabled.
        button_next.setDisable(true);
        button_previous.setDisable(true);
        button_auto.setDisable(true);

        button_import.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
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
                            enable();
                        }
                        break;

                }
                // if the diagram is not included in the switch case, check if the diagram is invalid
                DiagramCheck.ContainsDiagram(result);

            }});

        button_previous.setOnAction((ActionEvent event) ->{
            Net.push("{1, previous_message}");
            // Only go back if you can remove a message.
            if (DiagramView.getDiagramViewInView().getDraw().removeMessage())
                // Remove a line from the execution log.
                ExecutionLog.getInstance().bwd();
        });

        button_next.setOnAction((ActionEvent event) ->{
            Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", next_message}");
        });

        button_auto.setOnAction((ActionEvent event) ->{

            // Identify if the button is currently on or off.
            if (button_auto.getText().endsWith("|>")) { // Turn it on.
                play();

            } else { // Turn it off
                pause();
            }
        });

        menu.getChildren().add(button_import); // Adds the buttons to the menu box.
        menu.getChildren().add(button_previous); // Adds previous button
        menu.getChildren().add(button_auto); // Adds automate button
        menu.getChildren().add(button_next); // Adds next button

        return menu;
    }

    /**
     * Enables all the menu buttons.
     */
    public static void enable() {
        button_import.setDisable(false);
        button_auto.setDisable(false);
        button_next.setDisable(false);
        button_previous.setDisable(false);
    }

    /**
     * Starts automating the executing. Disables manual control.
     */
    public static void play() {
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

}
