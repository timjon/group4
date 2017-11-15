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
 * @version 1.2
 * @author Pontus Laestadius, Sebastian Fransson
 *  Collaborator Rashad Kamsheh
 */

public class Draw {

    private Canvas canvas; // Draws and handles graphical context
    private ArrayList<DiagramClass> diagramClasses = new ArrayList<>(); // Stores the classes
    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
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
        offset += 20;
        this.messages.add(new Message(diagramClasses.get(fromNode).getCoordinates(),
                diagramClasses.get(toNode).getCoordinates(), name, fromNode, toNode, offset, class_size));
        // Adds vertical lowering to the message when it is a self referencing message
        if (fromNode == toNode){
            // The offset for self referencing messages is slightly larger than the normal message offset
            // This way we handle the possibility of having two self referencing messages beneath each other directly
            offset += 40;
        }
    }

    /**
     * Removes the last message in the messages list.
     * @return true if it removed a message, false if the message list is empty.
     */
    public boolean removeMessage() {
        if (messages.isEmpty()) return false;
        // Removes vertical lowering to the message when it is self a referencing message
        if (messages.get(messages.size()-1).getFromNode()==messages.get(messages.size()-1).getToNode()){
            offset -=40;
        }
        messages.remove(messages.size()-1);
        offset -= 20;
        return true;
    }

    /**
     * Identifies if a message can be removed or not.
     * @return true if it can remove a message, false if the message list is empty.
     */
    public boolean canRemoveMessage() {
        return !messages.isEmpty();
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
        gc.drawImage(animatedBackground,0,0, this.canvas.getWidth(), this.canvas.getHeight());
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

    /**
     * @return the last message in the draw object.
     * @throws NullPointerException if there are no messages.
     */
    public Message getLastMessage() throws NullPointerException, ArrayIndexOutOfBoundsException {
        return this.messages.get(this.messages.size()-1);
    }
}
