package view.visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;

import javafx.scene.image.Image;

import javafx.scene.paint.Color;
import view.DiagramView;
import view.handlers.Animation;
import view.visuals.component.*;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * @version 2.0
 * @author Pontus Laestadius, Sebastian Fransson
 * Collaborator Rashad Kamsheh, Kosara Golemshinska, Isabelle Törnqvist
 */

public class Draw {

    private Canvas canvas; // Draws and handles graphical context

    private Canvas canvas_class; // Draws and handles class diagram graphical context.
    private Canvas canvas_deployment; // Draws and handles class diagram graphical context.

    private ArrayList<Renderable> allClasses = new ArrayList<>(); // Stores the classes

    private ArrayList<Renderable> allClassDiagramClasses = new ArrayList<>(); //Stores the classdiagram classes

    private ArrayList<Message> messages = new ArrayList<>(); // Stores the messages between nodes.
    private int offset; // Used for message ordering
    private int class_size = 0; // Used for message positioning
	
    //stores an animated gif file specifically made for this application, which contains an 8-bit animation of a sky/ocean view
    private static Image animatedBackground = new Image("resources/SkyGIF.gif");

    private static Image classDiagramBackground  = new Image("resources/grassland.png");

    /**
     * Constructor
     */
    public Draw(int w, int h) {
        canvas = new Canvas(w, h);
        canvas_class = new Canvas(0, 0);
        canvas_deployment = new Canvas(0,0);

        // TODO: remove Mock input data for class diagram
        addClassDiagramClass("1111");
        addClassDiagramClass("222");
        addClassDiagramClass("333");
        addClassDiagramClass("444");
        addClassDiagramClass("444");
        addClassDiagramClass("444");
        addClassDiagramClass("444");


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
    private int getHeight() {
        return (int)DiagramView.tabPane.getHeight();
    }

    /**
     * Gets the active canvas width
     * @return canvas width
     */
    private int getWidth() {
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
     * Adds class to arraylist, to be drawn
     * @param name
     */

    public void addClassDiagramClass (String name){
        allClassDiagramClasses.add(new ClassDiagramClass(name));
    }

    /**
     * Creates a message from and to given nodes with an attached name.
     */
    public void addMessage(int fromNode, int toNode, String name){
        offset += 40;
        this.messages.add(new Message(allClasses.get(fromNode).getCoordinates(),
                allClasses.get(toNode).getCoordinates(), name, fromNode, toNode, offset, class_size));
        // Adds vertical lowering to the message when it is a self referencing message
        if (fromNode == toNode){
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
            offset -= 40;
        }
        messages.remove(messages.size()-1);
        offset -= 40;
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
     * remakes the "Items", referring to messages and classes
     */
    private void renderItems() {
        renderClass();
        renderMessage();
        renderClassDiagramClass();
    }

    /**
     * redraws the simulation canvas with all elements.
     */
    public void redraw() {
        init(canvas);
        initClassDiagram(canvas_class.getGraphicsContext2D());

        renderItems();
        renderContainer();
        clear(canvas_deployment);
        canvas_deployment.getGraphicsContext2D().fillRect(0,0,canvas_deployment.getWidth(), canvas_deployment.getHeight());
    }

    /**
     * Initializes the drawing of class diagram
     * @param gc
     */
    void initClassDiagram(GraphicsContext gc){
        gc.clearRect(0,0,getWidth(), getHeight());
        // adds the background to class diagram canvas
        gc.drawImage(classDiagramBackground,0,0, this.canvas_class.getWidth(), this.canvas_class.getHeight());
    }

    /**
     * initializes the frame with an animated background.
     * @param canvas to get properties from.
     */
    private void init(Canvas canvas) {

        // Removes the content of the canvas.
        clear(canvas);

        // adds an animated gif file to the canvas with proper height and width.
        canvas.getGraphicsContext2D().drawImage(animatedBackground,0,0, this.canvas.getWidth(), this.canvas.getHeight());
    }

    /**
     * Clear the provided GraphicalContext.
     * @param canvas to get properties from.
     */
    private void clear(Canvas canvas) {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.clearRect(0,0,canvas.getWidth(), canvas.getHeight()); // Clears the canvas
    }

    /**
     * Renders the actor and classes in a new thread as well as the messages.
     */
    void renderContainer() {
        if (!DiagramView.inView(this)) return;
        GraphicsContext gc = canvas.getGraphicsContext2D();
        GraphicsContext graphicsContext = canvas_class.getGraphicsContext2D(); //content for class diagram
        for (Renderable r: allClasses)
            r.render(gc);
        for (Renderable r: messages)
            r.render(gc);
        //rendering of class diagram
        for (Renderable e: allClassDiagramClasses)
            e.render(graphicsContext);
    }

    /**
     * Updates the Renderables.
     */
    public void update() {
        for (Renderable r: allClasses)
            r.update();
        for (Renderable r: messages)
            r.update();
        for (Renderable r: allClassDiagramClasses)
            r.update();
    }

    /**
     * Renders the message when trying to resize the application.
     */
    private void renderMessage() {
        if(messages.size() == 0) return; // There are no messages in the list.
        for (Message message: messages) { //Messages exist and will now be be re-placed.
            Coordinates node1 = allClasses.get(message.getFromNode()).getCoordinates();
            Coordinates node2 = allClasses.get(message.getToNode()).getCoordinates();
            // Changes the coordinates of the messages.
            message.changeCoordinates(node1, node2, class_size);
        }
    }

    /**
     * checks a canvas properties to validate if it is in view or not.
     * @param canvas to check
     */
    private boolean inView(Canvas canvas) {
        return canvas.getWidth() == 0 && canvas.getHeight() == 0;
    }

    /**
     * Updates the class to fit the resized window.
     */
    private void renderClass() {
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
     * Places the classes in the class diagram
     */

    void renderClassDiagramClass() {
        if (allClassDiagramClasses.size() == 0) return;
        int space = ((int) this.canvas_class.getWidth()) / this.allClassDiagramClasses.size();

        int size = allClassDiagramClasses.size();
        int col = 3;
        int rows;
        int deduction = 0;
        //Find how many rows in this matrix
        //and the deduction
        if(size <= col){ rows = 1;}
        else  if (size % col == 0) {
            rows = size / col;
            deduction = 0;
        }
        else if  (size % col == 1){
            rows = ((size / col) + 1);
            deduction = 2;
        }
        else {
            rows = ((size / col) + 1);
            deduction = 2;
        }


        //Debugg
        System.out.println(" yooo Size: " + size + ", col: " + col + ", and rows: " + rows);


        for (int r = 0; r < rows; r++) {
            int y = (int) ((this.canvas_class.getHeight() / 2) + (r * 50));
            for (int c = 0; c < allClassDiagramClasses.size() - (col * r); c++) {
                System.out.println("r: " + r + ", c:" + c + "deduction: " + deduction);
                int x = space / 2 + (c * space);
                int element = (r * col) + c;
                System.out.println("element: " + element);
                allClassDiagramClasses.get(element).place(new Coordinates(x, y), (space) / 2);
            }
        }

    }









/*


        while (size >= 0) {
            int y = (int)((this.canvas_class.getHeight() / 2) + (rowCount * 5));
            for (int i = 0; i <= col; i++) {
                int x = space / 2 + (i * space);
                allClassDiagramClasses.get(i).place(new Coordinates(x, y), space / 2);
            }
            size -= col;
            rowCount++;
        }
    }








        int chunks = 3;

        if (allClassDiagramClasses.size() <= chunks) {
            for (int i = 0; i < allClassDiagramClasses.size(); i++) {
                int x = space / 2 + (i * space);
                int y = (int) ((this.canvas_class.getHeight() / 2));
                allClassDiagramClasses.get(i).place(new Coordinates(x, y), space / 2);
            }
        } else {

            for (int i = 0; i < allClassDiagramClasses.size() - chunks; i += chunks) {

                int y = (int) ((this.canvas_class.getHeight() / 2) + (i * 5));

                for (int j = i; j < chunks; j++) {

                    int x = space / 2 + (j * space);

                    allClassDiagramClasses.get(j).place(new Coordinates(x, y), (space) / 2);
                }
            }
        }
    }
*/

/*
        for(int i = 0; i < 3; i++) {
            int x = (space/2) + (i * space);
            int y = (int) ((this.canvas_class.getHeight()/2));
            allClassDiagramClasses.get(i).place(new Coordinates(x,y), (space)/2);
        }
        for(int i = 3; i < 6; i++) {
            int t = 0;
            int x = (space/2) + (t * space);
            int y = (int) ((this.canvas_class.getHeight() / 2) - space/2) ;
            allClassDiagramClasses.get(i).place(new Coordinates(x,y), (space)/2);
            t++;
        }



        for(int i = 0; i < allClassDiagramClasses.size(); i++) {
            for(int j = 0; j < allClassDiagramClasses.size()/2; j++){

                int x = (space/2) + (i * space);
                int y = (int) ((this.canvas_class.getHeight()/2) - space/2);
                allClassDiagramClasses.get(j).place(new Coordinates(x,y), (space)/2);
            }

           int y = (int) ((this.canvas_class.getHeight() / 2));
           int x = (space / 2) + ((i - 1) * space);




            if(i >= 3) {
                int t = i;
                y = (int) ((this.canvas_class.getHeight() / 2));
                x = (space / 2) + ((t - 1) * space);
            }

            if(i >= 6){
                int t = i;
                y = (int) ((this.canvas_class.getHeight() / 2) + space/2);
                x = (space / 2) + ((t - 1) * space);
            }*/

           // allClassDiagramClasses.get(i).place(new Coordinates(x,y), (space)/2);
        //}



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
}
