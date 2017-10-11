import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import visuals.Draw;

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
        ta.minHeight(1100);
        ta.setMaxWidth(302);
        ta.setPrefColumnCount(60);
        ta.setPrefRowCount(50);

        GridPane pane =  new GridPane(); // The "pane" containing the sequence-diagram.
        pane.setHgap(5);
        pane.setVgap(5);
        pane.add(btn_import, 1,0);
        pane.add(btn2, 2,        0);
        pane.add(ta, 1, 3, 20, 20);

        // Init's a draw object that handles graphical elements
        Draw draw = new Draw(1180, 790);

        // Replace AddClass with output from parser or something. TODO
        draw.addClass("test1"); // TODO remove, It's a test.
        draw.addClass("test2"); // TODO remove, It's a test.
        draw.addClass("test3"); // TODO remove, It's a test.
        draw.addClass("test4"); // TODO remove, It's a test.
        draw.addClass("test5 long name"); // TODO remove, It's a test.
        draw.addClass("test6"); // TODO remove, It's a test.
        draw.addClass("test7"); // TODO remove, It's a test.
        draw.addClass("test8"); // TODO remove, It's a test.
        draw.addClass("test9"); // TODO remove, It's a test.


        // Renders and displays the classes
        draw.render();
        draw.addMessage(0, 1, "Message 1"); //TODO Remove, Just a test.
        draw.addMessage(3, 4, "Message 2"); //TODO Remove, Just a test.
        pane.add(draw.getCanvas(), 22, 3, 20, 20);

        StackPane stack = new StackPane();

        stack.getChildren().add(pane);

        Scene main = new Scene(stack,1500, 837);

        primaryStage.setScene(main);
        primaryStage.setResizable(false);
        primaryStage.show();
    }
}