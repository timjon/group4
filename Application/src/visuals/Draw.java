package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import net.Net;

import javafx.scene.image.Image;

import visuals.handlers.Animation;

import java.util.ArrayList;

import static visuals.DiagramView.tabPane;

/**
 * @version 1.0
 * @author Pontus Laestadius, Sebastian Fransson
 *  -collaborator Rashad Kamsheh
 */

public class Draw {

    private Canvas canvas; // Draws and handles graphical context
    private ArrayList<DiagramClass> diagramClasses = new ArrayList<>(); // Stores the classes
    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
    private ArrayList<Trail> trails = new ArrayList<>(); //Stores the trails that appear after the dragons
    private int offset; // Used for message ordering
    private int class_size = 0; // Used for message positioning
	
    //stores an animated gif file specifically made for this application, which contains an 8-bit animation of a sky/ocean view
    private static Image animatedBackground = new Image("resources/SkyGIF.gif");

    /**
     * Constructor
     */
    Draw(int w, int h) {
        canvas = new Canvas(w, h);
    }

    /**
     * Gets the active canvas.
     * @return canvas
     */
    Canvas getCanvas() {
        return canvas;
    }

    /**
     * Gets the active canvas height.
     * @return canvas height
     */
    int getHeight() {
        return (int)canvas.getHeight();
    }

    /**
     * Gets the active canvas width
     * @return canvas width
     */
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
        offset += 8;
        this.messages.add(new Message(diagramClasses.get(fromNode).getCoordinates(),
                diagramClasses.get(toNode).getCoordinates(), name, fromNode, toNode, offset, class_size));

    }

    /**
     * Removes the last message in the messages list.
     * @return true if it removed a message, false if the message list is empty.
     */
    public boolean removeMessage() {
        if (messages.isEmpty()) return false;
        messages.remove(messages.size()-1);
        return true;
    }



    //TODO: STUFF--------------------------------------
    public void addTrail(int fromNode, int toNode, String name){
        offset += 7;
        this.trails.add(new Trail(diagramClasses.get(fromNode).getCoordinates(),
                diagramClasses.get(toNode).getCoordinates(), name, fromNode, toNode, offset, class_size));

    }

    /**
     * Removes the last message in the messages list.
     * @return true if it removed a message, false if the message list is empty.
     */
    public boolean removeTrail() {
        if (trails.isEmpty()) return false;
        trails.remove(trails.size()-1);
        return true;
    }


    /**
     * @param name class name to match.
     * @return the index the DiagramClass is located at.
     */
    public int findClassIndex(String name){

        // Iterate over the existing diagram classes
        for (int i = 0; i < diagramClasses.size(); i++) {

            if (diagramClasses.get(i).getName().equals(name))

                // Return the index in the array.
                return i;
        }

        // Return a number that can't exist if the class does not. Thus a negative number.
        return -1;
    }

    /**
     * Always renders with a new specific resolution.
     * @param w the new width of the canvas.
     * @param h the new height of the canvas.
     */
    void resize(double w, double h) {
        if (w == getWidth() && h == getHeight())
            return;
        canvas.setWidth(w);
        canvas.setHeight(h);
        redraw();
    }

    /**
     * remakes the "Items", referring to messages and classes
     */
    void renderItems() {
        renderClass();
        renderMessage();
        renderTrail();
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
     * initializes the simulation canvas with an animated 8-bit sky/ocean background .
     */
    void init() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.clearRect(0,0,getWidth(), getHeight()); // Clears the canvas
        // adds an animated gif file to the canvas with proper height and width
        gc.drawImage(animatedBackground,10,10, this.canvas.getWidth(), this.canvas.getHeight());
        Animation.setFramesPerSecond(3); // Sets the desired frames per second.
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
        for (Renderable r: trails)
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
        for (Renderable r: trails)
            r.update();
    }

    /**
     * Renders the message when trying to resize the application.
     */
    void renderMessage() {
        if(messages.size() == 0) return; // There are no messages in the list.
        if(this.messages.size() > 0) {
            for (Message message: messages) { //Messages exist and will now be be re-placed.
                Coordinates node1 = diagramClasses.get(message.getFromNode()).getCoordinates();
                Coordinates node2 = diagramClasses.get(message.getToNode()).getCoordinates();
                // Changes the coordinates of the messages.
                message.changeCoordinates(node1, node2, class_size);
            }
        }
    }

    //TODO: MORE STUFF
    /**
     * Renders blubblbubb
     */
    void renderTrail() {
        if(trails.size() == 0) return;
        if(this.trails.size() > 0) {
            for (Trail trail: trails) {
                Coordinates node1 = diagramClasses.get(trail.getFromNode()).getCoordinates();
                Coordinates node2 = diagramClasses.get(trail.getToNode()).getCoordinates();

                trail.changeCoordinates(node1, node2, class_size);
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
    public static void animate(boolean active) {
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

        if (nr > 4) {
            this.addMessage(3, 4, "Msg2");
            this.addMessage(4, 3, "Msg3");
        }

        //Trails
        this.addTrail(0, 1, "MSGFFS");
        if (nr > 4) {
            this.addTrail(3, 4, "DDD");
            this.addTrail(4, 3, "DDMDD");
        }


        animate(true); // Animates all example messages at once. in the real execution messages would be sent in iterations, so ordering is of no issue.
    }

    public static void temp_generate_diagram() { // Init's a draw object that handles graphical elements
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
