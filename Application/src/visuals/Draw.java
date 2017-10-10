package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.text.Text;

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
        gc.strokeRoundRect(0,0,w-1,h-1, 0,0);
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void addClass(String name) {
        this.classes_names.add(name);
    }

    /**
     * Renders the classes that have been added on to the Canvas.
     * Dynamically scales the size and position of all classes.
     * Draws the classes as Castles.
     * @author Pontus Laestadius
     */
    public void render() {

        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        int x_offset = 30;

        int width = (int) this.canvas.getWidth();        // The width of the canvas

        // The amount of space each class can use.
        int space = (width-x_offset*3)/this.classes_names.size();
        int size = space/2;

        int offset = 0;
        for (String n: this.classes_names) {
            int x = size+ (offset++*space) +x_offset;
            // Offsets half of the classes y coordinates.
            int y = (this.classes.size() % 2 == 0 ? 70:73);

            this.classes.add(new Coordinates(x,y));
            Image image = new Image("resources/castle.jpg");
            gc.drawImage(image, x, y, size/2, size/2);
            gc.fillText(n, x + size/8, y -15);
        }

    }
}
