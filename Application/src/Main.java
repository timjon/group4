import javafx.application.Application;
import javafx.beans.value.ChangeListener;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextArea;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import visuals.DiagramView;
import visuals.Draw;

import java.awt.*;
import java.util.Collection;

public class Main extends Application {
    public static void main(String[] args) {
        launch(args);
    }
    @Override
    public void start(Stage primaryStage) {

        primaryStage.setTitle("FUML");
        Button btn_import = new Button();
        btn_import.setText("Import");
        btn_import.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) { // Import button action handler.
                Collection<String> result = Import.file(primaryStage);
                if (result == null) return;

                // TODO remove print line and parse result (user story 5)
                System.out.println(result);
            }
        });

        Button btn2 = new Button();
        btn2.setText("Settings");
        btn2.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                System.out.println("Settings"); } // Settings button handler.
        });

        TextArea ta = new TextArea("Execution Log: \n " +  // Execution log text-box and it's current contents.
                "> Node1: sent a message to Node2 \n " +
                "> Node2: received message from Node1 \n " +
                "> Node2: sent a OK to Node1 \n " +
                "> Node1: received an OK from Node2");
        ta.setEditable(false);
        ta.setPrefColumnCount(60);
        ta.setPrefRowCount(50);
        int ta_width = 270;
        ta.setMaxWidth(ta_width);

        //Scales the application to the size of the window.
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        double width = screenSize.getWidth();
        double height = screenSize.getHeight();

        BorderPane borderpane = new BorderPane();

        TabPane tabPane = new TabPane();

        HBox hbox = new HBox();
        hbox.getChildren().add(btn_import);
        hbox.getChildren().add(btn2);
        borderpane.setTop(hbox);
        borderpane.setLeft(ta);

        borderpane.setCenter(tabPane);

        Scene main;
        if (width < 1400) { // full screen
            main = new Scene(borderpane,width, height);
            primaryStage.setMaximized(true);
        } else { // windowed
            main = new Scene(borderpane,1280, 720);
        }

        primaryStage.setScene(main);
        primaryStage.show();

        // TODO rename and improve.
        function_name_that_changes_properties_and_inits(primaryStage, tabPane);

    }

    public static void function_name_that_changes_properties_and_inits(Stage primaryStage, TabPane tabPane) {
        System.out.println("Tab pane: " + tabPane.getWidth() + "x" + tabPane.getWidth());

        // Init's a draw object that handles graphical elements
        Draw draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw.test(); // Only used to display an example.

        DiagramView dv = new DiagramView(draw, "diagram name");

        tabPane.getTabs().add(dv.getTab());

        // Renders and displays the classes
        draw.render();
        draw.addMessage(0, 1, "Message 1"); //TODO Remove, Just a test.
        draw.addMessage(3, 4, "Message 2"); //TODO Remove, Just a test.


        // Listener for when the window is resized.
        ChangeListener<Number> stageSizeListener = (obserable, oldVal, newVal) ->
        {
            // re-render elements.
            // Rip performance? TODO don't rip performance.
            for (DiagramView d: DiagramView.list) {
                System.out.println("1");
                d.resize(obserable.toString(), obserable.getValue().intValue());
            }

        };

        primaryStage.widthProperty().addListener(stageSizeListener);
        primaryStage.heightProperty().addListener(stageSizeListener);


    }
}