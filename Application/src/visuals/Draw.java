package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import net.Net;
import visuals.handlers.Animation;

import java.util.ArrayList;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.8
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas; // Draws and handles graphical context
    private ArrayList<DiagramClass> diagramClasses = new ArrayList<>(); // Stores the classes
    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
    private int offset; // Used for message ordering
    private int class_size = 0; // Used for message positioning

    /**
     * Constructor
     */
    Draw(int w, int h) {
        canvas = new Canvas(w, h);
    }

    Canvas getCanvas() {
        return canvas;
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
    public void addClass(String name) {
        diagramClasses.add(new DiagramClass(name));
    }

    /**
     * Creates a message from and to given nodes with an attached name.
     */
    public void addMessage(int fromNode, int toNode, String name){
        offset += 10;
        this.messages.add(new Message(diagramClasses.get(fromNode).getCoordinates(),
                diagramClasses.get(toNode).getCoordinates(), name, fromNode, toNode, offset, class_size));

    }


    /**
     *
     * @param name class name to match.
     * @return the index the DiagramClass is located at.
     */
    public int findClassIndex(String name){
        for (int i = 0; i < diagramClasses.size(); i++) {
            System.out.println("match: '" + diagramClasses.get(i).getName() + "' with '" + name + "'");
            if (diagramClasses.get(i).getName().equals(name))
                return i;
        }
        return -1;
    }

    // Always renders with a new specific resolution.
    void resize(double w, double h) {
        if (w == getWidth() && h == getHeight())
            return;
        canvas.setWidth(w);
        canvas.setHeight(h);
        redraw();
    }

    /**
     * remakes the "Items", reffering to messages and classes
     */
    void renderItems() {
        renderClass();
        renderMessage();
    }

    /**
     * redraws the simulation canvas with all elements.
     */
    public void redraw() {
        renderItems();
        init();
        renderContainer();
    }

    /**
     * initializes the simulation canvas with background colours.
     */
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

    /**
     * Renders classes in a new thread as well as the messages.
     */
    void renderContainer() {
        if (!DiagramView.inView(this)) return;
        GraphicsContext gc = canvas.getGraphicsContext2D();
        for (Renderable r: diagramClasses)
            r.render(gc);
        for (Renderable r: messages)
            r.render(gc);
    }

    /**
     * Updates the Renderables.
     */
    public void update() {
        for (Renderable r: diagramClasses)
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
                Coordinates node1 = diagramClasses.get(messages.get(i).getFromNode()).getCoordinates();
                Coordinates node2 = diagramClasses.get(messages.get(i).getToNode()).getCoordinates();
                messages.get(i).changeCoordinates(node1, node2, class_size);
            }
        }
    }

    /**
     * Updates the class to fit the resized window.
     */
    void renderClass() {
        if (diagramClasses.size() == 0) return; // There are no items to render
        int space = (getWidth())/this.diagramClasses.size(); // The amount of space each class can use.
        int size = space/2; // The size of the objects is half of it's given space.
        class_size = size/2;
        for(int i = 0; i < diagramClasses.size(); i++) {
            int x = size+ (i*space);
            int y = 25 +size/4;
            diagramClasses.get(i).place(new Coordinates(x,y), size);
        }
    }

    /**
     * Starts the global Animation thread for all Draw objects and views.
     */
    static void animate(boolean active) {
        if (active)
            (new Thread(new Animation())).start();
        else
            Animation.cancel();
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
        //Net.test();
        old_generate_diagram();
        Draw.animate(true);
    }

    public static void old_generate_diagram() {
        for (int i = 1; i <= 5; i++) {
            String name = "diagram " + i;
            DiagramView dv = new DiagramView(name);
            dv.getDraw().example_diagram(i*2); // Only used to display an example.
            tabPane.getTabs().add(dv.getTab());
        }
    }
}
