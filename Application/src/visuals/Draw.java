package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.util.ArrayList;

/**
 * @version 0.2
 * @author Pontus Laestadius
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Renderable> items = new ArrayList<>();
    private ArrayList<Class> classes = new ArrayList<>();

    public Canvas getCanvas() {
        return canvas;
    }

    public Draw(int w, int h) {
        this.canvas = new Canvas(w, h);
        GraphicsContext gc = this.canvas.getGraphicsContext2D();

        // Draws a single pixel border around the canvas.
        gc.setFill(Color.GREY);
        gc.strokeRoundRect(0,-1,w,h+1, 0,0);
        gc.setFill(Color.BLACK);
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void addClass(String name) {
        this.classes.add(new Class(name));
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void add(Renderable item) {
        this.items.add(item);
    }

    // Draws a Message.
    public void addMessage(int fromNode, int toNode, String name){ // TODO accommodate dynamic message adding.
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        // fromClass coordinates.
        int x1 = items.get(fromNode).getCoordinates().getX();
        int y1 = items.get(fromNode).getCoordinates().getY();
        // toClass coordinates.
        int x2 = items.get(toNode).getCoordinates().getX();
        int y2 = items.get(toNode).getCoordinates().getY();

        Message message = new Message(x1+30, x2, y1+20, y2, gc, name);

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

        // If there are any classes that have not been processed.
        renderClass();

        // Renders all the items on the canvas.
        for (Renderable r: this.items) {
            r.render(gc);
        }
    }

    public void renderMessage() {
        // stuff.
    }


    public void renderClass() {
        int x_offset = 30;
        int width = (int) this.canvas.getWidth();        // The width of the canvas
        if (this.classes.size() > 0) {

            // The amount of space each class can use.
            int space = (width-x_offset*3)/this.classes.size();
            int size = space/2;

            int offset = 0;
            for (Class c: this.classes) {
                int x = size+ (offset++*space) +x_offset;
                // Offsets half of the classes y coordinates.
                int y = (this.classes.size() % 2 == 0 ? 70:73);

                c.place(new Coordinates(x,y), size);
                this.add(c);
            }
            // Empties the list.
            this.classes = new ArrayList<>();
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

    // TODO remove, It's a test.
    public void test() {

        // Classes
        this.addClass("test1");
        this.addClass("test2");
        this.addClass("test3");
        this.addClass("test4");
        this.addClass("test5 long name");
        this.addClass("test6");
        this.addClass("test7");
        this.addClass("test8");
        this.addClass("test9");

        // Messages

    }

}
