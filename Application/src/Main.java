import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.scene.Group;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.scene.shape.ArcType;

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
            public void handle(ActionEvent event) {
                Collection<String> result = Import.file(primaryStage);
                if (result == null) return;

                

				// Check if the JSON file contains a supported diagram type.
                System.out.println(DiagramCheck.ContainsDiagram(result));
                // Parse the element if it contains a sequence_diagram
				if (DiagramCheck.ContainsDiagram(result) == "sequence_diagram" ){
                for (String element : result ) {
                    Parser.Parser(element);
                }
            }
		}});

        Button btn2 = new Button();
        btn2.setText("Settings");
        btn2.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                System.out.println("Settings");
            }
        });

        TextArea ta = new TextArea("Execution Log: \n " +
                "> Node1: sent a message to Node2 \n " +
                "> Node2: received message from Node1 \n " +
                "> Node2: sent a OK to Node1 \n " +
                "> Node1: received an OK from Node2");
        ta.setEditable(false);
        ta.minHeight(1100);
        ta.setMaxWidth(302);
        ta.setPrefColumnCount(60);
        ta.setPrefRowCount(50);

        GridPane pane =  new GridPane();
        pane.setHgap(5);
        pane.setVgap(5);
        pane.add(btn_import, 1,0);
        pane.add(btn2, 2,        0);
        pane.add(ta, 1, 3, 20, 20);

        Group root = new Group();
        Canvas canvas = new Canvas(600, 300);
        GraphicsContext gc = canvas.getGraphicsContext2D();
        drawShapes(gc);
        pane.add(canvas, 22, 3, 20, 20);

        StackPane stack = new StackPane();

        stack.getChildren().add(pane);

        Scene main = new Scene(stack,1200, 840);

        primaryStage.setScene(main);
        primaryStage.show();
    }


    private void drawShapes(GraphicsContext gc) {
        gc.setFill(Color.GREEN);
        gc.setStroke(Color.BLUE);
        gc.setLineWidth(5);
        gc.strokeLine(40, 10, 10, 40);
        gc.fillOval(10, 60, 30, 30);
        gc.strokeOval(60, 60, 30, 30);
        gc.fillRoundRect(110, 60, 30, 30, 10, 10);
        gc.strokeRoundRect(160, 60, 30, 30, 10, 10);
        gc.fillArc(10, 110, 30, 30, 45, 240, ArcType.OPEN);
        gc.fillArc(60, 110, 30, 30, 45, 240, ArcType.CHORD);
        gc.fillArc(110, 110, 30, 30, 45, 240, ArcType.ROUND);
        gc.strokeArc(10, 160, 30, 30, 45, 240, ArcType.OPEN);
        gc.strokeArc(60, 160, 30, 30, 45, 240, ArcType.CHORD);
        gc.strokeArc(110, 160, 30, 30, 45, 240, ArcType.ROUND);
        gc.fillPolygon(new double[]{10, 40, 10, 40},
                new double[]{210, 210, 240, 240}, 4);
        gc.strokePolygon(new double[]{60, 90, 60, 90},
                new double[]{210, 210, 240, 240}, 4);
        gc.strokePolyline(new double[]{110, 140, 110, 140},
                new double[]{210, 210, 240, 240}, 4);
    }
}