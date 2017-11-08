import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Scene;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import javafx.scene.image.Image;

import net.Net;
import visuals.DiagramView;
import visuals.ExecutionLog;
import model.Menu;
import visuals.handlers.Resizer;

import java.awt.*;

import static visuals.DiagramView.tabPane;

public class Main extends Application {
    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        primaryStage.getIcons().add(new Image("resources/logo.png"));

        primaryStage.setTitle("FUML");

        tabPane = new TabPane();

        // When you switch tabs, renders the tabs again after they were frozen.
        tabPane.getSelectionModel().selectedItemProperty().addListener(
                new ChangeListener<Tab>() {
                    @Override
                    public void changed(ObservableValue<? extends Tab> ov, Tab t, Tab t1) {
                        for (DiagramView diagramView: DiagramView.diagramViews) {
                            diagramView.redraw();
                        }
                        DiagramView diagramView = DiagramView.getDiagramViewInView();
                        diagramView.focus();
                    }
                }
        );

        ExecutionLog executionLog = new ExecutionLog();

        BorderPane borderpane = new BorderPane(); // Initializes a new BorderPane that holds all Elements.

        Menu menu_ = new Menu();

        HBox menu = menu_.get(primaryStage); // A HBox holds items horizontally like a menu.
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

        /*
        Resources used to figure out the following code:
        https://docs.oracle.com/javafx/2/threads/jfxpub-threads.htm
        https://stackoverflow.com/questions/21083945/how-to-avoid-not-on-fx-application-thread-currentthread-javafx-application-th
        https://docs.oracle.com/javase/8/javafx/api/javafx/application/Platform.html#runLater-java.lang.Runnable-
         */
        // Sets a flag to true to terminate if the last window has been closed.
        Platform.setImplicitExit(true);

        // Run Later adds the code to an event queue where they get processed when the program reaches it.
        // This is done to avoid changing UI elements on noneFX threads and is used in several places in the application.
        Platform.runLater(() -> {
            Resizer.init(primaryStage); // Starts a listener for handling resizing of the window.
            Net.init();
        });
    }
}
