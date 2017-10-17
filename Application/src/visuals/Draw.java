package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.util.ArrayList;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.6
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<Message> messages = new ArrayList<>();

    Canvas getCanvas() {
        return canvas;
    }

    Draw(int w, int h) {
        canvas = new Canvas(w, h);
        (new Thread(new AnimationHandler(this))).start();
    }

    int getHeight() {
        return (int)canvas.getHeight();
    }

    int getWidth() {
        return (int)canvas.getWidth();
    }

    /**
     * Draws a Class on the provided canvas.
     */
    private void addClass(String name) {
        classes.add(new Class(name));
    }

    // Draws a Message.
    public void addMessage(int fromNode, int toNode, String name){ // TODO
        GraphicsContext gc = canvas.getGraphicsContext2D();
        // fromClass coordinates.
        Coordinates node1 = classes.get(fromNode).getCoordinates();
        // toClass coordinates.
        Coordinates node2 = classes.get(toNode).getCoordinates();

        Message message = new Message(node1, node2, name);
        messages.add(message);
    }

    void render() {
        init();
        renderClass();
        renderMessage();
        renderContainer();
    }

    // Always renders with a new specific resolution.
    void resize(double w, double h) {
        if (w == getWidth() && h == getHeight())
            return;
        canvas.setWidth(w);
        canvas.setHeight(h);
        redraw();
    }

    void redraw() {
        renderClass();
        renderMessage();
        init();
        long t1 = System.currentTimeMillis();
        renderContainer();
        System.out.println(System.currentTimeMillis()-t1);
    }

    void init() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.clearRect(0,0,getWidth(), getHeight()); // Clears the canvas
        gc.setFill(Color.GREY); // Sets the color to GREY
        gc.strokeRoundRect(0,-1,getWidth(),getHeight()+1, 0,0); // Draws a border
        gc.setFill(Color.BLACK); // Resets the color to BLACK
    }

    void renderContainer() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        for (Class c: classes) // TODO new thing!
            c.update();

        Thread c = new Thread(new RenderHandler(gc,classes));
        c.start();
        Thread m = new Thread(new RenderHandler(gc,messages));
        m.start();

        try {
            m.join();
            c.join();
        } catch (InterruptedException e) {
            System.err.println(e.toString());
        }
    }

    void renderMessage() {
        //What? confusion is real...
        //---------------------------
        // Ok, what you do is do all computing stuff here.
        // Then let magic do the rest.
        // see renderClass for reference.
    }

    void renderClass() {
        if (classes.size() == 0) return; // There are no items to render
        int space = (getWidth())/this.classes.size(); // The amount of space each class can use.
        int size = space/2; // The size of the objects is half of it's given space.
        for(int i = 0; i < classes.size(); i++) {
            int x = size+ (i*space);
            int y = (i % 2 == 0 ? 70:71);
            classes.get(i).place(new Coordinates(x,y), size);
        }
    }

    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------
    //----------------------------TEST CODE BELOW------------------------------------
    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------

    void example_diagram() {
        // Classes
        for (int i = 0; i < 10; i++)
            this.addClass("test" + i);
        // Messages
        //this.addMessage(0, 1, "Message 1");
        //this.addMessage(3, 4, "Message 2");
    }

    public static void temp_generate_diagram() { // Init's a draw object that handles graphical elements
        Draw draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw.example_diagram(); // Only used to display an example.
        DiagramView dv = new DiagramView(draw, "diagram name");
        tabPane.getTabs().add(dv.getTab());
        draw.render(); // Renders and displays items
    }
}
