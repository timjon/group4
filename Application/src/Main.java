import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import javafx.scene.image.Image;

import net.Net;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;
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
		primaryStage.getIcons().add(new Image("resources/logo.png"));
        Button btn_import = new Button();
        btn_import.setText("Import");
        btn_import.setOnAction((ActionEvent event) -> {
            Collection<String> result = Import.file(primaryStage);
            if (result == null) return;
            // Check if the JSON file contains a supported diagram type.
            System.out.println(DiagramCheck.ContainsDiagram(result));
            // TODO remove print line and parse result (user story 5)
            System.out.println(result);
        });

        Button btn2 = new Button();
        btn2.setText("Settings");
        btn2.setOnAction((ActionEvent event) ->{
            System.out.println("Settings");  // Settings button handler.
        });

        tabPane = new TabPane();

        // When you switch tabs, renders the tabs again after they were frozen.
        tabPane.getSelectionModel().selectedItemProperty().addListener(
                new ChangeListener<Tab>() {
                    @Override
                    public void changed(ObservableValue<? extends Tab> ov, Tab t, Tab t1) {
                        for (DiagramView dv: DiagramView.diagramViews) {
                            dv.redraw();
                        }
                        DiagramView dv = DiagramView.getDiagramViewInView();
                        if (dv != null) dv.focus();
                    }
                }
        );

        ExecutionLog executionLog = new ExecutionLog();

        BorderPane borderpane = new BorderPane(); // Initializes a new BorderPane that holds all Elements.
        HBox menu = new HBox(); // A HBox holds items horizontally like a menu.
        menu.getChildren().add(btn_import); // Adds the buttons to the menu box.
        menu.getChildren().add(btn2);
        borderpane.setTop(menu); // Gets the menu to be at the top of the window.
        borderpane.setCenter(tabPane); // Sets the TabPane to the center/main focus of the application.
        borderpane.setLeft(executionLog.getContainer()); // Sets the Execution log to be on the left side.

        Scene main;
        //Scales the application to the size of the window or displays it in a maximized view.
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        double width = screenSize.getWidth(); // Gets the width of the screen.
        double height = screenSize.getHeight(); // Gets the height of the screen.
        if (width < 1400) { // When the width is smaller than a certain width, Start as maximized.
            main = new Scene(borderpane,width, height);
            primaryStage.setMaximized(true);
        } else { // All other cases, the width should be set to 720p.
            main = new Scene(borderpane,1280, 720);
        }

        primaryStage.setScene(main);
        primaryStage.show();

        // https://docs.oracle.com/javafx/2/threads/jfxpub-threads.htm
        // https://stackoverflow.com/questions/21083945/how-to-avoid-not-on-fx-application-thread-currentthread-javafx-application-th
        // https://docs.oracle.com/javase/8/javafx/api/javafx/application/Platform.html#runLater-java.lang.Runnable-
        Platform.setImplicitExit(true);
        Platform.runLater(() -> {
            Draw.temp_generate_diagram(); // TODO replace with actual parsing.
            Resizer.init(primaryStage); // Starts a listener for handling resizing of the window.
            Net.init();
        });

    }

}