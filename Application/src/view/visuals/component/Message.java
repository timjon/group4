package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.text.Font;
import view.visuals.Renderable;

import java.util.ArrayList;

/**
 * Class for creating the messages to pass between "classes".
 * @author Sebastian Fransson
 * Collaborator Rashad Kamsheh, Isabelle Törnqvist, Pontus Laestadius, Tim Jonasson
 * @version 4.4
 */
public class Message implements Renderable {
    private String name;
    private int size;
    private Coordinates coordinates , node1, node2; // The coordinates of the nodes that the message is supposed to pass between.
    private int fromNode, toNode;
    private int offset; // offset for the message "ordering".
    private int class_size; // identifier for the current class size so that messages can scale.
    private int animationBounds = 0; //Starts and stops the animation at the correct bounds.
    private boolean animating = true; // State of the animation. Beginning or ending.
    private boolean switchImage; // Keeps track of which image to show.
    private double messageScale = 1.5;
    private int selfCallCounter = 1; // Used for the hardcoded trajectory of self referencing message.
    private double trailScale = 3.5; //Scale of the trail image
    private ArrayList<Trail> trails = new ArrayList<>(); //Stores the trails that appear after the dragons
    private boolean directionSwitched = false; // if the trail should be flipped depending on the direction of the message
    private boolean staticIndicator = false;    // Static indicator used for the execution log.

    //Images for dragon animation.
    private static Image dragonMessage = new Image("resources/DragonBro.png"); //Wings Up
    private static Image dragonMessage2 = new Image("resources/DragonBro2.png"); //Wings Down
    private static Image dragonMessageRev = new Image("resources/DragonBroRev.png"); //Rev Wings Up
    private static Image dragonMessageRev2 = new Image("resources/DragonBroRev2.png"); //Rev Wings Down

    private static Image trail = new Image("resources/cloud1.png");    //Image for trail animation
    private static Image arrow = new Image("resources/trail.png");    //Image for trail arrow
    private static Image flippedArrow = new Image("resources/rotated-trail.png");    //Image that stores trail arrow


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
        return fromNode + " -> " + toNode + " | " + name;
    }


    /**
     * @return if it is still animating or not.
     */
    public boolean isAnimating() {
        return animating;
    }

    /**
     * Method for animating the message and setting the image size for messages in both directions.
     */

    @Override
    public void update() {

        int move = class_size/6;

        // If we are animating
        if (animating) {

            //Checks if we are supposed to keep animating, set animationBounds according to how diagramClasses are scaled.


            // Sending a message.
            if(node2.getX() > node1.getX()) {

                if ((animationBounds += move) > this.node2.getX() - this.node1.getX()) {
                    animating = false;
                }

                // Sending a return message.
            } else if(node1.getX() > node2.getX()){

                if((animationBounds -= move) < (this.node2.getX())  - this.node1.getX()) {
                    animating = false;
                }

                // Self calls
            } else /*if (node1.getX() == node2.getX())*/ {

                switch (selfCallCounter) {
                    case 1:
                        // Move the message to the right of the class
                        if ((animationBounds += move) > this.class_size) {
                            // Original state of the dragon's flying direction
                            directionSwitched = !directionSwitched;
                            selfCallCounter = 2;
                        }
                        break;

                    case 2:
                        // Multiple increments to lower the message vertically
                        offset += 24;
                        selfCallCounter = 3;
                        break;

                    case 3:
                        // move back to starting point
                        if ((animationBounds -= move) < 1) {
                            animating = false;
                        }
                        break;

                    //default case
                    default :
                        selfCallCounter = 1;
                }
            }
        }

        // If it's being viewed in the past and not currently animating.
        if (staticIndicator) {
            // Reset direction.
            directionSwitched = false;
            // Remove all trails
            trails.clear();
            // Which direction is it pointing at determines original location.
            animationBounds = 0;
            // Set it's animation state to true.
            animating = true;
            //Check if the message is a self-call and reset the offset.
            if(fromNode == toNode) {
                offset -= 24;
            }
            //Resets counter for self-calls.
            selfCallCounter = 1;
            // Resets static indicator.
            staticIndicator = false;

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

    public void resizeTrail(int oldClassSize){
        //Checks if there is an existing trail for this message

        //If there is no trail
        if(trails.size() == 0){
            return;
        }
        //Calculates the new size of the trail image and the difference between the window size
        double trailsize = class_size/trailScale;
        double class_size_change = (double)class_size/oldClassSize;

        //Makes sure the trails doesnt move too much to the left
        int firstX = (int)(trails.get(0).getXcoordinate() * class_size_change);
        int trailOffset;
        //Calculates the offset depending on the direction of the message
        trailOffset = node1.getX() - firstX;

        //resizes all the trails
        for(Trail t: trails){
            t.resize(trailsize, class_size_change, trailOffset);
        }

    }

    /**
     * Renders a message on the canvas using the provided coordinates.
     * Updates the message image and placing for simulation.
     */
    @Override
    public void render(GraphicsContext gc){
        // Draws the message.
        renderDefault(gc);
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

    /**
     * @param staticIndicator true to permanently repeat the animation. False to disable the animation.
     */
    public void setStatic(boolean staticIndicator)  {
        this.staticIndicator = staticIndicator;
        if (!staticIndicator)
            this.animating = false;
    }

    /**
     * Default message rendering.
     * @param gc The GraphicalContext to display the info on.
     */
    public void renderDefault(GraphicsContext gc) {

        // Does not apply to self calls.
        if (fromNode != toNode)
            directionSwitched = fromNode > toNode;

        if(trails.size() != 0) {
            //Draws all trails except the first and last one
            for (int i = 1; i < trails.size() - 1; i++) {
                // Points to the first index of the trail array
                Trail currentTrail = trails.get(i);
                gc.drawImage(trail, currentTrail.getXcoordinate(), (currentTrail.getYcoordinate() + 18), currentTrail.getWidth(), currentTrail.getHeight());
            }

            if(trails.size() > 1) {
                Trail last = trails.get(trails.size() -1);
                //Puts an arrow on the last location of the trail array depending on the direction
                gc.drawImage(directionSwitched?flippedArrow:arrow, last.getXcoordinate(), (last.getYcoordinate() + 18), last.getWidth(), last.getHeight());
            }
        }
        //fromNode Coordinates.
        int x1 = this.node1.getX();
        int y1 = this.node1.getY();
        //toNode Coordinates.
        int x2 = this.node2.getX();
        // int y2 = this.node2.getY(); //Not used atm.

        y1 += offset; // Sets an offset from the previous message.
        
        // Checks if we are supposed to be animating the message.
        if(animating) {

            // Image to draw.
            Image dragon = switchImage?dragonMessage:dragonMessage2;
            double height = class_size/messageScale;
            double width = class_size/messageScale;

            // If it's switched direction.
            if (directionSwitched)
                dragon = switchImage?dragonMessageRev2:dragonMessageRev;

            //Sets the message text centered above the "dragon".
            gc.fillText(this.name, x1+animationBounds, y1 + (this.class_size - 2)); // Message description.

            // Draws the trails every other frame.
            if (switchImage)
                this.trails.add(new Trail(x1+animationBounds, y1 +(this.class_size),class_size/trailScale, height));

            // Draws the dragon.
            gc.drawImage(dragon, x1 + animationBounds, y1 + (this.class_size), width, height);

            // Swaps the image.
            switchImage = !switchImage;
        } else {

            // Sets the message above the trail.
            gc.fillText(this.name, x1-(x1-x2)/2 -this.name.length()*2, y1 +this.class_size*1.2); // Message description.

        }
    }

    /**
     * Getter for the name of the message.
     * @return name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Uses the coordinates and size passed as arguments.
     * to place the message on the canvas.
     * @param coordinates of the message.
     * @param size of the message.
     */
    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;

    }

    public int getClass_size(){
        return this.class_size;
    }
}
