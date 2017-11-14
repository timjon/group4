package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;

/**
 * Class for creating the messages to pass between "classes".
 * @author Sebastian Fransson
 * -collaborator Rashad Kamsheh
 * @version 2.0
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
    private double messageScale = 1.5;
    private int selfCallCounter = 1; // Used for the hardcoded trajectory of self referencing message.
    private boolean switchDirection = false; // Keeps track whether a the dragon's flying direction should be switched

    //Images for dragon animation.
    private static Image dragonMessage = new Image("resources/DragonBro.png"); //Wings Up
    private static Image dragonMessage2 = new Image("resources/DragonBro2.png"); //Wings Down
    private static Image dragonMessageRev = new Image("resources/DragonBroRev.png"); //Rev Wings Up
    private static Image dragonMessageRev2 = new Image("resources/DragonBroRev2.png"); //Rev Wings Down

    // Curve
    private double curve_increment = 1.05;
    private double curve = 1;

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
     * @return coordinates
     */
    @Override
    public Coordinates getCoordinates() { return coordinates; }

    /**
     * Formatting method.
     * @return null
     */
    @Override
    public String format() {
        return null;
    }


    /**
     * @return if it is still animating or not.
     */
    public boolean isKeepAnimating() {
        return keepAnimating;
    }

    /**
     * Method for animating the message and setting the image size for messages in both directions.
     */
    @Override
    public void update() {
        curve *= curve_increment;

            //Checks if we are supposed to keep animating, set animationBounds according to how diagramClasses are scaled.
           if(keepAnimating && node2.getX() > node1.getX()) { // Sending a message.
               if ((animationBounds += (class_size/6 +curve)) > this.node2.getX() - this.node1.getX())
                   keepAnimating = false;
           }
            //Checks if we are supposed to keep animating, set animationBounds according to how diagramClasses are scaled.
           else if(keepAnimating && node1.getX() > node2.getX()){ // Sending a return message.
               if((animationBounds -= (class_size/6 +curve)) < (this.node2.getX())  - this.node1.getX())
                   keepAnimating = false;
           }
        //Checks if we are supposed to keep animating, set animationBounds according to how diagramClasses are scaled.
        else if (keepAnimating && node1.getX() == node2.getX()) { //Sending a self referencing message
            switch (selfCallCounter) {
                case 1:
                    switchDirection = true; // Switch state of the dragon's flying direction
                    // Move the message to the right of the class
                    if ((animationBounds += (class_size / 6)) > this.class_size) {
                        switchDirection = false; // Original state of the dragon's flying direction
                        selfCallCounter = 2;
                    }
                    break;

                case 2:
                    // Multiple increments to lower the message vertically
                    for (int i = 0; i < 3; i++) {

                        offset += 4;
                    }
                    selfCallCounter = 3;
                    break;

                case 3:
                    // move back to starting point
                    if ((animationBounds -= (class_size / 6)) < 1) {
                        keepAnimating = false;
                    }
                    break;

                //default case
                default :
                    selfCallCounter =1;
            }
        }
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
        // int x2 = this.node2.getX(); //Not used atm.
        // int y2 = this.node2.getY(); //Not used atm.

        y1 += offset; // Sets an offset from the previous message.

        // Checks if we are supposed to be animating the message.
        if(keepAnimating) {
            //Sets the message text centered above the "dragon".
            gc.fillText(this.name, x1+animationBounds, y1 + (this.class_size - 2)); // Message description.

            // Checks if up image is supposed to be shown and if we are sending a message and not a return.
            if (switchImage && fromNode < toNode) {
                //sets the dimensions of the dragon according to the current class size.
                gc.drawImage(dragonMessage, x1 + animationBounds,
                        y1 + (this.class_size), class_size/messageScale, class_size/messageScale); //State Wings Up.
                switchImage = false;
            }

            // Checks if down image is supposed to be shown and if we are sending a message and not a return.
            else if (!switchImage && fromNode < toNode) {
                //sets the dimensions of the dragon according to the current class size.
                gc.drawImage(dragonMessage2, x1 + animationBounds,
                        y1 + (this.class_size), class_size/messageScale, class_size/messageScale); //State Wings Down.
                switchImage = true;
            }

            // Handling the switch of the dragon's flying direction when visualising self calls
            // Checks if up image is supposed to be shown and if the direction of flying dragon should be switched.
            else if (switchDirection && switchImage) {
                //sets the dimensions of the dragon according to the current class size.
                gc.drawImage(dragonMessage, x1 + animationBounds,
                        y1 + (this.class_size), class_size / messageScale, class_size / messageScale); //State Wings Down.
                //TODO: add a trail below for full integration with the trail feature

                switchImage = false;
            }
            // Checks if up image is supposed to be shown and if the direction of flying dragon should be switched.
            else if (switchDirection && !switchImage) {
                //sets the dimensions of the dragon according to the current class size.
                gc.drawImage(dragonMessage2, x1 + animationBounds,
                        y1 + (this.class_size), class_size / messageScale, class_size / messageScale); //State Wings Down.
                switchImage = true;
            }

            //Checks if up image is supposed to be shown. if this one is used it is a return message.
            else if(switchImage){
                //sets the dimensions of the dragon according to the current class size.
                gc.drawImage(dragonMessageRev, x1 + animationBounds,
                        y1 + (this.class_size), class_size/messageScale, class_size/messageScale); //State Wings up.
                switchImage = false;
            }

            //Checks if down image is supposed to be shown. if this one is used we are sending a return message.
            else{
                //sets the dimensions of the dragon according to the current class size.
                gc.drawImage(dragonMessageRev2, x1 + animationBounds,
                        y1 + (this.class_size), class_size/messageScale, class_size/messageScale); //State Wings Down.
                switchImage = true;
            }

        }
    }

    /**
     * Getter method for retrieving the beginning node of a message
     * @return fromNode
     */
    public int getFromNode(){
        return fromNode;
    }

    /**
     * Getter method for retrieving the end node of a message
     * @return toNode
     */
    public int getToNode(){
        return toNode;
    }

}
