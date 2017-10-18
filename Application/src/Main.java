import javafx.application.Application;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextArea;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

import visuals.DiagramView;
import visuals.Draw;
import visuals.handlers.Resizer;

import java.awt.*;
import java.util.Collection;

import static visuals.DiagramView.tabPane;

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


        tabPane = new TabPane();

        // When you switch tabs, renders the tabs again after they were frozen.
        tabPane.getSelectionModel().selectedItemProperty().addListener(
                new ChangeListener<Tab>() {
                    @Override
                    public void changed(ObservableValue<? extends Tab> ov, Tab t, Tab t1) {
                        for (DiagramView dv: DiagramView.list) {
                            dv.redraw();
                        }
                    }
                }
        );

        BorderPane borderpane = new BorderPane();
        HBox hbox = new HBox();
        hbox.getChildren().add(btn_import);
        hbox.getChildren().add(btn2);
        borderpane.setTop(hbox);
        borderpane.setLeft(ta);
        borderpane.setCenter(tabPane);

        Scene main;
        //Scales the application to the size of the window or displays it in a maximized view.
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        double width = screenSize.getWidth();
        double height = screenSize.getHeight();
        if (width < 1400) { // full screen
            main = new Scene(borderpane,width, height);
            primaryStage.setMaximized(true);
        } else { // windowed
            main = new Scene(borderpane,1280, 720);
        }

        primaryStage.setScene(main);
        primaryStage.show();

        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
            System.err.println(e.toString());
            System.out.println(e.toString());
        }

        Draw.temp_generate_diagram(); // TODO replace with actual parsing.
        try {
            Thread.sleep(200); 
        } catch (InterruptedException e) {
            System.err.println(e.toString());
            System.out.println(e.toString());
        }
        Resizer.init(primaryStage);
    }
}