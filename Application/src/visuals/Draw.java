package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.scene.shape.ArcType;

import java.util.ArrayList;

public class Draw {

    private Canvas canvas;
    private ArrayList<Coordinates> classes = new ArrayList<>();
    private ArrayList<String> classes_names = new ArrayList<>();

    public Canvas getCanvas() {
        return canvas;
    }

    public Draw(int w, int h) {
        this.canvas = new Canvas(w, h);
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        gc.strokeRoundRect(0,0,w-1,h-1,0,0);
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void addClass(String name) {
        this.classes_names.add(name);
    }

    public void render() {

        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        int x_offset = 30;

        // The width of the canvas
        int width = (int) this.canvas.getWidth();
        // The amount of space each class can use.
        int space = (width-x_offset*3)/this.classes_names.size();

        int size = space/2;

        // Anything multiplied by PI is correct.
        //int size = (int) (internal_size*Math.PI) ;

        int offset = 0;
        for (String n: this.classes_names) {
            int x = size+ (offset++*space) +x_offset;
            // Offsets half of the classes y coordinates.
            int y = (this.classes.size() % 2 == 0 ? 70:73);

            this.classes.add(new Coordinates(x,y));
            gc.fillRoundRect(x, y, size/2, size/2, size/2, size/2);

        }

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

class Coordinates {
    private int x;
    private int y;

    public Coordinates(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }
}
