package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;

/**
 * @version 0.2
 * @author Pontus Laestadius
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<String> classes_names = new ArrayList<>();

    public Canvas getCanvas() {
        return canvas;
    }

    public Draw(int w, int h) {
        this.canvas = new Canvas(w, h);
        GraphicsContext gc = this.canvas.getGraphicsContext2D();

        // Draws a single pixel border around the canvas.
        gc.strokeRoundRect(0,0,w-1,h-1, 0,0);
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void addClass(String name) {
        this.classes_names.add(name);
    }

    // Draws a Message.
    public void addMessage(int fromNode, int toNode){
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        // fromClass coordinates.
        int x1 = classes.get(fromNode).getCoordinates().getX();
        int y1 = classes.get(fromNode).getCoordinates().getY();
        // toClass coordinates.
        int x2 = classes.get(toNode).getCoordinates().getX();
        int y2 = classes.get(toNode).getCoordinates().getY();

        Message message = new Message(x1+30, x2, y1+20, y2, gc);

        message.createMessage();

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

            this.classes.add(new Class(new Coordinates(x,y), size, n));
        }

        for (Class c: this.classes) {
            c.render(gc);
        }
    }



    /**
     * renders an Object on the Canvas.
     * @param instance a Object which implements Renderable
     */
    public void render(Renderable instance) {
        instance.render(this.canvas.getGraphicsContext2D());
    }

    /**
     * renders several Object on the Canvas.
     * @param instance a Object which implements Renderable
     */
    public void render(Renderable instance[]) {
        for (Renderable i: instance) {
            i.render(this.canvas.getGraphicsContext2D());
        }
    }

}
