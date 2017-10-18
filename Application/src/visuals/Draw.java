package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import visuals.handlers.Animation;
import visuals.handlers.Render;

import java.util.ArrayList;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.6
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
    private int offset = 30;
    private String name;

    public String getName() {
        return name;
    }

    Canvas getCanvas() {
        return canvas;
    }

    Draw(String n, int w, int h) {
        canvas = new Canvas(w, h);
        name = n;
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

    /**
     * Creates a message from and to given nodes with an attached name.
     */
    public void addMessage(int fromNode, int toNode, String name){
        offset += 10;
        this.messages.add(new Message(classes.get(fromNode).getCoordinates(),
                classes.get(toNode).getCoordinates(), name, fromNode, toNode, offset));
    }

    void render() {
        init();
        renderItems();
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

    void renderItems() {
        renderClass();
        renderMessage();
    }

    public void redraw() {
        renderItems();
        init();
        renderContainer();
    }

    void init() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.clearRect(0,0,getWidth(), getHeight()); // Clears the canvas

        gc.setFill(Color.ALICEBLUE); // Sets the color to GREY
        int split = getHeight()/2 +getHeight()/4;
        gc.fillRect(0,0,getWidth(),split);
        gc.setFill(Color.CORNFLOWERBLUE); // Sets the color to GREY
        gc.fillRect(0,split,getWidth(), getHeight());
        gc.setFill(Color.BLACK); // Resets the color to BLACK
    }

    void renderContainer() {
        if (!DiagramView.inView(name)) return;
        GraphicsContext gc = canvas.getGraphicsContext2D();
        Thread c = new Thread(new Render(gc,classes));
        c.start();

        for (Renderable r: messages)
            r.render(gc);

        try {
            c.join();
        } catch (InterruptedException e) {
            System.err.println(e.toString());
            System.out.println(e.toString());
        }
    }

    /**
     * Updates the Renderables.
     */
    public void update() {
        for (Renderable r: classes)
            r.update();
        for (Renderable r: messages)
            r.update();
    }

    /**
     * Renders the message when trying to resize the application.
     */
    void renderMessage() {
        if(this.messages.size() > 0) {
            for (int i = 0; i < messages.size(); i++) {
                Coordinates node1 = classes.get(messages.get(i).getFromNode()).getCoordinates();
                Coordinates node2 = classes.get(messages.get(i).getToNode()).getCoordinates();
                messages.get(i).changeCoordinates(node1, node2);
            }
        }
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

    void animate() {
        (new Thread(new Animation(this))).start();

    }
    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------
    //----------------------------TEST CODE BELOW------------------------------------
    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------

    void example_diagram() {
        // Classes
        for (int i = 0; i < 8; i++)
            this.addClass("test" + i);
        // Messages
        this.addMessage(0, 1, "Message 1");
        this.addMessage(3, 4, "Message 2");
    }

    public static void temp_generate_diagram() { // Init's a draw object that handles graphical elements
        String name = "diagram name";
        Draw draw = new Draw(name, (int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw.example_diagram(); // Only used to display an example.
        DiagramView dv = new DiagramView(draw, name);
        draw.animate();
        tabPane.getTabs().add(dv.getTab());
        draw.render(); // Renders and displays items

        name = "diagram name2";

        Draw draw2 = new Draw(name, (int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw2.example_diagram(); // Only used to display an example.
        DiagramView dv2 = new DiagramView(draw2, name);
        draw2.animate();
        tabPane.getTabs().add(dv2.getTab());
        draw2.render(); // Renders and displays items
    }
}
