package visuals;

import javafx.scene.canvas.GraphicsContext;

/**
 * Class for creating the messages to pass between "classes".
 * @author Sebastian Fransson
 */
public class Message implements Renderable{
    String name;
    private Coordinates coordinates , node1, node2; // The coordinates of the nodes that the message is supposed to pass between.
    private int fromNode, toNode;

    /**
     * Constructor
     */
    public Message(Coordinates node1, Coordinates node2, String name, int fromNode, int toNode){
        this.node1 = node1;
        this.node2 = node2;
        this.name = name;
        this.fromNode = fromNode;
        this.toNode = toNode;
    }

    /**
     * Retrieves the coordinates from the Message object.
     */
    @Override
    public Coordinates getCoordinates() { return coordinates; }

    /**
     * Formatting method.
     */
    @Override
    public String format() {
        return null;
    }

    /**
     * Method for animating the message.
     */
    @Override
    public void update() {
        // Animate
    }

    /**
     * Gets the new coordinates from resizing the application.
     */
    public void changeCoordinates(Coordinates node1, Coordinates node2){
        this.node1 = node1;
        this.node2 = node2;
    }

    /**
     * Renders a message on the canvas using the provided coordinates.
     */
    @Override
    public void render(GraphicsContext gc){

        //fromNode Coordinates.
        int x1 = this.node1.getX();
        int y1 = this.node1.getY();
        //toNode Coordinates.
        int x2 = this.node2.getX();
        int y2 = this.node2.getY();
        gc.strokeLine(x1+10, y1+50, x2, y1+50); // Message Line.
        gc.fillText(this.name, x1+25, y1+40); // Message description.


    }

    /**
     * Getter method for retrieving the beginning node of a message
     */
    public int getFromNode(){
        return fromNode;
    }

    /**
     * Getter method for retrieving the end node of a message
     */
    public int getToNode(){
        return toNode;
    }

}