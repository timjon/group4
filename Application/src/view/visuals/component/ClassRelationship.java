package view.visuals.component;

import javafx.scene.SnapshotParameters;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

import java.util.ArrayList;

/**
 * Class for simulating the relationships of the class diagram
 *
 * @author Rashad Kamsheh
 * Collaborator: Pontus Laestadius
 * @version 1.3
 */

public class ClassRelationship implements Renderable {
    
    //ImageView that contains the arrow, used ImageView to be able to rotate
    private static ImageView arrow = new ImageView("resources/road-arrow.png");
    private Coordinates coordinates; //coordinates
    private Coordinates fromNode, toNode;
    private int size; //size of the road sprite
    public int class1Index = 0;
    public int class2Index = 0;

    /**
     * Constructor
     * @param fromNode superclass
     * @param toNode subclass
     * @param size size of the road sprite
     */
    public ClassRelationship(Coordinates fromNode, Coordinates toNode, int size) {
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    /**
     * Initialiser for rendering the road sprites between a super class and a sub class
     * @param fromNode superclass
     * @param toNode subclass
     * @param size size of the road sprite
     */
    public void updateLocation(Coordinates fromNode, Coordinates toNode, int size) {
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    /**
     * sets the parents of the subclass and the subclass
     * @param one first class
     * @param two second class
     */
    public void setParents(int one, int two) {
        this.class1Index = one;
        this.class2Index = two;
    }

    /**
     * renders each relationship between the superclass and the subclass
     * @param gc Graphics context
     */
    @Override
    public void render(GraphicsContext gc) {

        // If we got no size to work with.
        if (size == 0) return;
        // Coordinates
        int startingPointX = fromNode.getX() -size/2; // Points to the middle of the super class, used for horizontal lines
        int startingPointY = fromNode.getY() +size/2; // Points to the middle of the super class, user for vertical lines
        int endingPointX = toNode.getX() -size/2; // Points to the middle of the sub class, used for horizontal lines
        int endingPointY = toNode.getY() +size/2; // Points to the middle of the sub class, used for horizontal lines
        // Deltas of distances
        int XDistance = endingPointX - startingPointX; //if this is negative, ending point is more to the left than starting point on X axis
        int YDistance = endingPointY - startingPointY; // if this is negative, same as above, for Y axis
        // Offsets
        int XOffset = 0; // Horizontal offset
        int YOffset = 0; // Vertical offset

        // Rotate inheritance arrow
        arrow.setRotate(getArrowAngle(XDistance, YDistance));
        // Converts imageView into image
        SnapshotParameters snapshotParameters = new SnapshotParameters();
        snapshotParameters.setFill(Color.TRANSPARENT);
        Image rotatedArrow = arrow.snapshot(snapshotParameters, null);
        //arrow size for the arrow head at the end of the road
        double arrowSize = size*1.2;
        if (Math.abs(YDistance) > 0 && Math.abs(XDistance) > 0){
            arrowSize = size*1.2+5;
        }

        // used to decide the amount of road sprites to be drawn
        int stepsParameter = Math.abs(XDistance)/size == 0?1:Math.abs(XDistance)/size;
        // used to increment the vertical offset
        int yInc = YDistance/stepsParameter;
        // used to increment the horizontal offset
        int xInc = (XDistance<0?-1:1)*size;

        double roadSize = size*1.15;
        gc.setFill(Color.GREY);

        // Straight lines
        if (stepsParameter == 1) {
            for (int i = 0; i < Math.abs(YDistance/size); i++) {
                YOffset += size * (YDistance<0?-1:1);
                int x = startingPointX + XOffset;
                int y = startingPointY + YOffset;
                // Draw a road
                gc.fillRoundRect(x,y,roadSize,roadSize,roadSize/2,roadSize/2);
            }

            // Draw an arrow at the end of the road
            gc.drawImage(rotatedArrow,
                    endingPointX +roadSize/2 -arrowSize/4,
                    endingPointY -size*4,
                    arrowSize/1.5, arrowSize/1.5);

            // Most lines
        } else {
            // This for loop is responsible for drawing the road sprites
            for (int i = 0; i < stepsParameter; i++) {
                XOffset += xInc;
                YOffset += yInc;
                int x = startingPointX + XOffset;
                int y = startingPointY + YOffset;
                // Draw a road
                gc.fillRoundRect(x,y,roadSize,roadSize,roadSize/2,roadSize/2);
            }

            // Draw an arrow at the end of the road
            gc.drawImage(rotatedArrow,
                    endingPointX -xInc*4 +roadSize/2 -arrowSize/4,
                    endingPointY -yInc*4 +roadSize/2 -arrowSize/4,
                    arrowSize/1.5, arrowSize/1.5);

        }



    }

    /**
     * Method get the proper angle to rotate the inheritance arrow
     *
     * @param XDistance the difference between 2 points on the X axis
     * @param YDistance the difference between 2 points on the Y axis
     * @return arrowAngle
     */
    private int getArrowAngle(int XDistance, int YDistance) {

        // Defaults to north
        int arrowAngle = 0; // used to rotate the inheritance arrow to 8 different directions

        // pointing east
        if (XDistance > 0 && YDistance == 0) {
            arrowAngle = 90;
        }
        // pointing west
        else if (XDistance < 0 && YDistance == 0) {
            arrowAngle = 270;
        }
        // pointing south
        else if (XDistance == 0 && YDistance > 0) {
            arrowAngle = 180;
        }
        // pointing south east
        else if (XDistance > 0 && YDistance > 0) {
            arrowAngle = 135;
        }
        // pointing north east
        else if (XDistance > 0 && YDistance < 0) {
            arrowAngle = 45;
        }
        // pointing north west
        else if (XDistance < 0 && YDistance < 0) {
            arrowAngle = 315;
        }
        // pointing south west
        else if (XDistance < 0 && YDistance > 0) {
            arrowAngle = 225;
        }

        return arrowAngle;
    }

    /**
     * Sets the coordinates and size, used to place the class
     *
     * @param coordinates
     * @param size
     */
    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }

    /**
     * @return name
     */
    @Override
    public String getName() {
        return "";
    }

    /**
     * not used because no animations are implemented
     */
    @Override
    public void update() {
    }

    /**
     * @return coordinates
     */
    @Override
    public Coordinates getCoordinates() {
        return coordinates;
    }

    /**
     * @return format
     */
    @Override
    public String format() {
        return "";
    }

}