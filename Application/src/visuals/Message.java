package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.scene.image.Image;
import visuals.handlers.Animation;

/**
 * Class for creating the messages to pass between "classes".
 * @author Sebastian Fransson
 * @version 1.0
 */
public class Message implements Renderable{
    private String name;
    private Coordinates coordinates , node1, node2; // The coordinates of the nodes that the message is supposed to pass between.
    private int fromNode, toNode;
    private int offset; // offset for the message "ordering".
    private int class_size; // identifier for the current class size so that messages can scale.
    private int animationBounds = 0; //Starts and stops the animation at the correct bounds.
    private boolean keepAnimating = true; // State of the animation. Beginning or ending.
    private boolean switchImage; // Keeps track of which image to show.

    //Images for dragon animation.
    private static Image dragonMessage = new Image("resources/DragonBro.png"); //Wings Up
    private static Image dragonMessage2 = new Image("resources/DragonBro2.png"); //Wings Down
    private static Image dragonMessageRev = new Image("resources/DragonBroRev.png"); //Rev Wings Up
    private static Image dragonMessageRev2 = new Image("resources/DragonBroRev2.png"); //Rev Wings Down

    /**
     * Constructor
     */
    public Message(Coordinates node1, Coordinates node2, String name, int fromNode, int toNode, int offset, int size){
        this.node1 = node1;
        this.node2 = node2;
        this.name = name;
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.offset = offset;
        this.class_size = size;
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
     * Method for animating the message and setting the image size for messages in both directions.
     */
    @Override
    public void update() {
        try {
           if(keepAnimating == true && node2.getX() > node1.getX()) { // Sending a message.
               if ((animationBounds += class_size/6) > this.node2.getX() - this.node1.getX())
                   keepAnimating = false;
           }

           else if(keepAnimating == true && node1.getX() > node2.getX()){ // Sending a return message.
               if((animationBounds -= class_size/6) < (this.node2.getX())  - this.node1.getX())
                   keepAnimating = false;
           }
        } catch (Exception e) {e.printStackTrace();}
    }

    /**
     * Gets the new coordinates from resizing the application and redraws messages with these coordinates.
     */
    public void changeCoordinates(Coordinates node1, Coordinates node2, int class_size){
        this.node1 = node1;
        this.node2 = node2;
        this.class_size = class_size;
    }

    /**
     * Renders a message on the canvas using the provided coordinates.
     * Updates the message image and placing for simulation.
     */
    @Override
    public void render(GraphicsContext gc){
        //fromNode Coordinates.
        int x1 = this.node1.getX();
        int y1 = this.node1.getY();
        //toNode Coordinates.
        int x2 = this.node2.getX();
        int y2 = this.node2.getY();

        y1 += offset; // Sets an offset from the previous message.

        if(keepAnimating==true) {
            gc.fillText(this.name, x1+animationBounds, y1 + (this.class_size - 2)); // Message description.
            if (switchImage == true && fromNode < toNode) {
                gc.drawImage(dragonMessage, x1 + animationBounds,
                        y1 + (this.class_size), class_size/1.5, class_size/1.5); //Message State Wings Up.
                switchImage = false;
            } else if (switchImage == false && fromNode < toNode) {
                gc.drawImage(dragonMessage2, x1 + animationBounds,
                        y1 + (this.class_size), class_size/1.5, class_size/1.5); //Message state Wings Down.
                switchImage = true;
            }
            else if(switchImage==true){
                gc.drawImage(dragonMessageRev, x1 + animationBounds,
                        y1 + (this.class_size), class_size/1.5, class_size/1.5); //Return Message State Wings Up.
                switchImage = false;
            }
            else{
                gc.drawImage(dragonMessageRev2, x1 + animationBounds,
                        y1 + (this.class_size), class_size/1.5, class_size/1.5); // Return Message State Wings Down.
                switchImage = true;
            }

        }
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
