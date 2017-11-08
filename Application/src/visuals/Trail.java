package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;
import javafx.scene.image.Image;
import visuals.handlers.Animation;

/**
 * Shows the trails of the messages being sent.
 * @author Isabelle Törnqvist
 * @version 1.0
 */

public class Trail implements Renderable {
    private Coordinates coordinates , node1, node2;
    private int nodeSend, nodeReceive;
    private int class_size;
    private boolean keepAnimating = true;
    private int animationBounds = 0;
    private int offset;
    private double messageScale = 1.5;

    //Smoke pic
    private static Image smoke = new Image("resources/Cloud1.png");

    //Curve, ??
    private double curve = 1;
    private double curve_increment = 1.05;

    /**
     * TODO: comments
     * @param node1
     * @param node2
     * @param nodeSend
     * @param nodeReceive
     * @param size
     */
    public Trail(Coordinates node1, Coordinates node2, int nodeSend, int nodeReceive, int size){
        this.node1 = node1;
        this.node2 = node2;
        this.nodeSend = nodeSend;
        this.nodeReceive = nodeReceive;
        this.class_size = size;
        this.offset = offset;
    }

    /**
     * Formatting
     * @return
     */
    @Override
    public String format() {
        return null;
    }

    /**
     * Gets coordinates
     * @return
     */


    /**
     * TODO: STOLEN (more like borrowed) from Message.java, way to fix this?
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

    }


    @Override
    public Coordinates getCoordinates() {
        return coordinates;
    }

    public void changeCoordinates(Coordinates node1, Coordinates node2, int class_size){
        this.node1 = node1;
        this.node2 = node2;
        this.class_size = class_size;
    }

    @Override
    public void render(GraphicsContext gc) {
        int node1X = this.node1.getX();
        int node1Y = this.node1.getY();

        //Moving along the Y axis?? is this where its done?
        //No, should probably be x axis?
        //node1Y += offset;

        if(keepAnimating){

            if(nodeSend < nodeReceive){
                gc.drawImage(smoke, ((node1X + 1) + animationBounds), ((node1Y + 1) + (this.class_size)),
                        class_size/messageScale, class_size/messageScale);
            }

            else{
                gc.drawImage(smoke, ((node1X + 1) + animationBounds),
                        ((node1Y + 1) + (this.class_size)), class_size/messageScale, class_size/messageScale); //State Wings Down.

            }

        }

    }
    /**
     * Getter method for retrieving the beginning node of a message
     * @return nodeSend
     */
    public int getFromNode(){
        return nodeSend;
    }

    /**
     * Getter method for retrieving the end node of a message
     * @return nodeReceive
     */
    public int getToNode(){
        return nodeReceive;
    }

}

