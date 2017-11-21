package view.visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;

import javafx.scene.image.Image;

import view.DiagramView;
import view.handlers.Animation;
import view.visuals.component.*;

import java.util.ArrayList;

/**
 * @version 1.3
 * @author Pontus Laestadius, Sebastian Fransson
 * Collaborator Rashad Kamsheh, Kosara Golemshinska
 */

public class Draw {

    private Canvas canvas; // Draws and handles graphical context

    private Canvas canvas_class; // Draws and handles class diagram graphical context.
    private Canvas canvas_deployment; // Draws and handles class diagram graphical context.

    private ArrayList<Renderable> allClasses = new ArrayList<>(); // Stores the classes
    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
    private int offset; // Used for message ordering
    private int class_size = 0; // Used for message positioning
	
    //stores an animated gif file specifically made for this application, which contains an 8-bit animation of a sky/ocean view
    private static Image animatedBackground = new Image("resources/SkyGIF.gif");

    /**
     * Constructor
     */
    public Draw(int w, int h) {
        canvas = new Canvas(w, h);
        canvas_class = new Canvas(0,0);
        canvas_deployment = new Canvas(0,0);
    }


    /**
     * @param canvas to be used as class diagram.
     */
    public void addClassDiagram(Canvas canvas) {
        this.canvas_class = canvas;
    }

    /**
     * Gets the active canvas.
     * @return canvas
     */
    public Canvas getCanvas() {
        return canvas;
    }

    /**
     * Gets the class canvas.
     * @return canvas
     */
    public Canvas getCanvas_class() {
        return canvas_class;
    }

    /**
     * Gets the deployment canvas.
     * @return canvas
     */
    public Canvas getCanvas_deployment() {
        return canvas_deployment;
    }

    /**
     * Gets the active canvas height.
     * @return canvas height
     */
    int getHeight() {
        return (int)DiagramView.tabPane.getHeight();
    }

    /**
     * Gets the active canvas width
     * @return canvas width
     */
    int getWidth() {
        return (int)DiagramView.tabPane.getWidth();
    }
    
    /**
     * Draws the actor class on the canvas.
     */
    public void addActor(String name) {
    	allClasses.add(new ActorClass(name));
    }

    /**
     * Draws a Class on the provided canvas.
     */
    public void addClass(String name) {
        allClasses.add(new DiagramClass(name));  // Class gets added to the end of the array list.
    }

    /**
     * Creates a message from and to given nodes with an attached name.
     */
    public void addMessage(int fromNode, int toNode, String name){
        offset += 25;
        this.messages.add(new Message(allClasses.get(fromNode).getCoordinates(),
                allClasses.get(toNode).getCoordinates(), name, fromNode, toNode, offset, class_size));
        // Adds vertical lowering to the message when it is a self referencing message
        if (fromNode == toNode){
            offset += 25;
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
            offset -= 25;
        }
        messages.remove(messages.size()-1);
        offset -= 25;
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
        for (int i = 0; i < allClasses.size(); i++) {

            if (allClasses.get(i).getName().equals(name))

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
    public void resize(double w, double h) {
        if (w == getWidth() && h == getHeight())
            return;

        canvas.setHeight(h);
        canvas_class.setHeight(h);
        canvas_deployment.setHeight(h);

        canvas.setWidth(w);
        canvas_class.setWidth(w);
        canvas_deployment.setWidth(w);

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
        init(canvas.getGraphicsContext2D());
        renderContainer();
    }

    /**
     * initializes the simulation canvas with an animated 8-bit sky/ocean background .
     */
    void init(GraphicsContext gc) {
        gc.clearRect(0,0,getWidth(), getHeight()); // Clears the canvas
        // adds an animated gif file to the canvas with proper height and width
        gc.drawImage(animatedBackground,0,0, this.canvas.getWidth(), this.canvas.getHeight());
    }


    /**
     * Renders the actor and classes in a new thread as well as the messages.
     */
    void renderContainer() {
        if (!DiagramView.inView(this)) return;
        GraphicsContext gc = canvas.getGraphicsContext2D();
        for (Renderable r: allClasses)
            r.render(gc);
        for (Renderable r: messages)
            r.render(gc);
    }

    /**
     * Updates the Renderables.
     */
    public void update() {
        for (Renderable r: allClasses)
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
                Coordinates node1 = allClasses.get(message.getFromNode()).getCoordinates();
                Coordinates node2 = allClasses.get(message.getToNode()).getCoordinates();
                // Changes the coordinates of the messages.
                message.changeCoordinates(node1, node2, class_size);
            }
        }
    }

    /**
     * Updates the class to fit the resized window.
     */
    void renderClass() {
        if (allClasses.size() == 0) return; // There are no items to render
        int space = ((int)this.canvas.getWidth())/this.allClasses.size(); // The amount of space each class can use.
        int size = space/2; // The size of the objects is half of it's given space.
        class_size = size/2;
        for(int i = 0; i < allClasses.size(); i++) {
            int x = size+ (i*space);
            int y = 25 +size/4;
            allClasses.get(i).place(new Coordinates(x,y), size);
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
    public Message getLastMessage() throws ArrayIndexOutOfBoundsException {
        return getMessage(this.messages.size()-1);
    }

    /**
     * Return a message for the given index.
     * @param index the index.
     * @return a message.
     */
    public Message getMessage(int index) throws ArrayIndexOutOfBoundsException {
        if (this.messages.size() == 0 || index >= this.messages.size())
            throw new ArrayIndexOutOfBoundsException("Index: " + index + "Size: " + this.messages.size());
        return this.messages.get(index);
    }

    /**
     * Resets all messages but the last one to be not static.
     */
    public void resetCurrentTime() {
        for (int i = messages.size()-1; i > 0; i--)
            messages.get(i -1).setStatic(false);
    }

    /**
     * Resets all messages to not be static.
     */
    public void resetStatic() {
        for (Message message: messages)
            message.setStatic(false);
    }

}
