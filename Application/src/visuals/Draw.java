package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Observable;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.4
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Renderable> items = new ArrayList<>(); // Why are we collecting everything here? Isn't it just inconvenient?
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<Message> messages = new ArrayList<>();

    Canvas getCanvas() {
        return canvas;
    }

    Draw(int w, int h) {
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
    private void addClass(String name) {
        this.classes.add(new Class(name));
    }

    private void add(Renderable item) {
        this.items.add(item);
    }

    // Draws a Message.
    public void addMessage(int fromNode, int toNode, String name){ // TODO
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        // fromClass coordinates.
        Coordinates node1 = items.get(fromNode).getCoordinates();
        // toClass coordinates.
        Coordinates node2 = items.get(toNode).getCoordinates();

        Message message = new Message(node1, node2, name);
        message.render(gc);
    }

    void render() {
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        renderClass();
        renderMessage();
        for (Renderable r: this.items)
            r.render(gc);
    }

    // TODO experimental feature
    // Always renders with a new specific resolution.
    void resize(int w, int h) {
        if (w == (int)this.canvas.getWidth() && h == (int)this.canvas.getHeight() || this.items.size() < 1) {
            return;
        } // Return if there is nothing to change/display.
        this.canvas.setHeight(h);
        this.canvas.setWidth(w);
        rerender();
    }

    // Re renders all items.
    private void rerender() {
        ArrayList<Renderable> cpy = this.items;
        this.items = new ArrayList<>();

        for (int i = 0; i < cpy.size(); i++){
            Renderable item = cpy.get(i);
            if (item instanceof Class) {
                this.classes.add((Class) cpy.get(i));
            } else if (item instanceof Message) {
                this.messages.add((Message) this.items.get(i));
            } else {
                System.out.println("Unknown class: " + cpy.get(i).getClass());
            }
        }
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        gc.clearRect(0,0,this.canvas.getWidth(),this.canvas.getHeight());
        gc.setFill(Color.GREY);
        gc.strokeRoundRect(0,-1,this.canvas.getWidth(),this.canvas.getHeight()+1, 0,0);
        gc.setFill(Color.BLACK);
        render();
    }

    void renderMessage() {
        //What? confusion is real...
        //---------------------------
        // Ok, what you do is do all computing stuff here.
        // And add the message to this.add(message)
        // Then let magic do the rest.
    }

    void renderClass() {
        int x_offset = (int) (this.canvas.getWidth()/100);
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

    void example_diagram() {
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
        this.addMessage(0, 1, "Message 1"); //TODO Remove, Just a test.
        this.addMessage(3, 4, "Message 2"); //TODO Remove, Just a test.
    }

    public static void temp_generate_diagram() {
        // Init's a draw object that handles graphical elements
        Draw draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw.example_diagram(); // Only used to display an example.
        DiagramView dv = new DiagramView(draw, "diagram name");
        tabPane.getTabs().add(dv.getTab());
        // Renders and displays the classes
        draw.render();
    }
}
