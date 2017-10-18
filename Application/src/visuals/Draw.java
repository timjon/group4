package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import visuals.handlers.Animation;
import visuals.handlers.Render;

import java.util.ArrayList;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.65
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
    private int offset = 30;
    private int size;
    private String name;
    private int class_size = 0;

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
                classes.get(toNode).getCoordinates(), name, fromNode, toNode, offset, classes.get(fromNode).getSize()));

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
        class_size = size;
        for(int i = 0; i < classes.size(); i++) {
            int x = size+ (i*space);
            int y = (i % 2 == 0 ? 70:71);
            classes.get(i).place(new Coordinates(x,y), size);
        }
    }

    void animate() {
        //(new Thread(new Animation(this))).start(); // TODO animations are not for this sprint!
    }
    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------
    //----------------------------TEST CODE BELOW------------------------------------
    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------

    void example_diagram(int nr) {
        // Classes
        for (int i = 0; i < nr; i++)
            this.addClass("Class " + i);
        // Messages
        this.addMessage(0, 1, "Msg1");
        if (nr > 4)
            this.addMessage(3, 4, "Msg2");
    }

    public static void temp_generate_diagram() { // Init's a draw object that handles graphical elements
        for (int i = 1; i <= 10; i++) {
            String name = "diagram " + i;
            DiagramView dv = new DiagramView(name);
            dv.getDraw().example_diagram(i*2); // Only used to display an example.
            tabPane.getTabs().add(dv.getTab());
        }
    }
}
