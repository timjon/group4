package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.scene.shape.ArcType;

public class Draw {

    private Canvas canvas;
    private String[][] classes;

    public Canvas getCanvas() {
        return canvas;
    }

    public Draw(int w, int h) {
        this.canvas = new Canvas(w, h);
        this.classes = new String[w][h];
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void drawClass(int x, int y, String name) {
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        gc.fillRoundRect(x, y, 30, 30, 10, 10);
        this.classes[x][y] = name;
    }

    /*
    private void drawShapes(GraphicsContext gc) {
        gc.setFill(Color.GREEN);
        gc.setStroke(Color.BLUE);
        //gc.setLineWidth(5);
       // gc.strokeLine(40, 10, 10, 40);
        gc.fillOval(10, 60, 30, 30);
        gc.strokeOval(60, 60, 30, 30);
        gc.strokeRoundRect(160, 60, 30, 30, 10, 10);
    }
    */

}
