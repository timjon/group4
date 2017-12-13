package view.visuals.component;

import javafx.scene.SnapshotParameters;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

/**
 * Class for simulating the relationships of the class diagram
 *
 * @author Rashad Kamsheh
 * Collaborator: Pontus Laestadius
 * @version 1.2
 */

public class ClassRelationship implements Renderable {
    
    //Image for the road sprite which are used to resemble the class diagram relationships
    private static Image road = new Image("resources/roadSprite.png");
    //ImageView that contains the arrow, used ImageView to be able to rotate
    private static ImageView arrow = new ImageView("resources/road-arrow.png");
    private Coordinates coordinates; //coordinates
    private Coordinates fromNode, toNode;
    private int size; //size of the allowed class space
    public int class1Index = 0;
    public int class2Index = 0;


    // Constructor
    public ClassRelationship(Coordinates fromNode, Coordinates toNode, int size) {
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    // Initialiser for rendering the road sprites between a super class and a sub class
    public void init(Coordinates fromNode, Coordinates toNode, int size) {
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    // To solve the issue with all the different cases of linking the classes, we used Bresenham’s Line Algorithm
    // http://csunplugged.org/wp-content/uploads/2014/12/Lines.pdf

    /**
     * Used to initialise the parameters of Bresenham’s Line Algorithm
     *
     * @param XDistance the difference between 2 points on the X axis
     * @param YDistance the difference between 2 points on the Y axis
     * @return bresenhamsParametersArray
     */
    private int[] initBresenhamsParameters(int XDistance, int YDistance) {
        int[] bresenhamsParametersArray = new int[4];
        bresenhamsParametersArray[1] = 2 * Math.abs(YDistance); // A
        bresenhamsParametersArray[2] = bresenhamsParametersArray[1] - 2 * Math.abs((XDistance)); // B
        bresenhamsParametersArray[3] = bresenhamsParametersArray[1] - Math.abs((XDistance)); // P

        return bresenhamsParametersArray;
    }


    public void setParents(int one, int two) {
        this.class1Index = one;
        this.class2Index = two;
    }

    @Override
    public void render(GraphicsContext gc) {

        // If we got no size to work with.
        if (size == 0) return;

        // Coordinates
        int startingPointX = fromNode.getX(); // Points to the middle of the super class, used for horizontal lines
        int startingPointY = fromNode.getY(); // Points to the middle of the super class, user for vertical lines
        int endingPointX = toNode.getX(); // Points to the middle of the sub class, used for horizontal lines
        int endingPointY = toNode.getY(); // Points to the middle of the sub class, used for horizontal lines
        // Deltas od distances
        int XDistance = endingPointX - startingPointX; //if this is negative, ending point it more to the left than starting point on X axis
        int YDistance = endingPointY - startingPointY; // if this is negative, same as above, for Y axis
        // Offsets
        int XOffset = 0; // Vertical offset
        int YOffset = 0; // Vertical offset

        int mmmmmmmmmmagic = Math.abs(XDistance)/size == 0?1:Math.abs(XDistance)/size;
        int Yinc = YDistance/mmmmmmmmmmagic;
        int magic = (XDistance<0?-1:1)*size;

        // This for loop is responsible for drawing the road sprites
        for (int i = 0; i < mmmmmmmmmmagic; i++) {
            XOffset += magic;
            YOffset += Yinc;
            int x = startingPointX + XOffset;
            int y = startingPointY + YOffset;

            // Draw road sprites
            gc.drawImage(road, x, y, size*1.2, size*1.2);
        }

        // Rotate inheritance arrow
        arrow.setRotate(getArrowAngle(XDistance, YDistance));
        // Converts imageView into image
        SnapshotParameters snapshotParameters = new SnapshotParameters();
        snapshotParameters.setFill(Color.TRANSPARENT);
        Image rotatedImage = arrow.snapshot(snapshotParameters, null);
        // Draw an arrow at the end of the road
        gc.drawImage(rotatedImage, startingPointX + XOffset -magic*2, startingPointY + YOffset -Yinc*2, size, size);
    }

    /**
     * Method get the proper angle to rotate the inheritance arrow
     *
     * @param XDistance the difference between 2 points on the X axis
     * @param YDistance the difference between 2 points on the Y axis
     * @return arrowAngle
     */
    private int getArrowAngle(int XDistance, int YDistance) {

        int arrowAngle = 0; // used to rotate the inheritance arrow to 8 different directions

        // pointing east
        if (XDistance > 0 && YDistance == 0) {
            arrowAngle += 90;
        }

        // pointing west
        else if (XDistance < 0 && YDistance == 0) {
            arrowAngle += 270;
        }

        // pointing south
        else if (XDistance == 0 && YDistance > 0) {
            arrowAngle += 180;
        }

        // pointing north
        else if (XDistance == 0 && YDistance < 0) {
            arrowAngle += 0;
        }

        // pointing south east
        else if (XDistance > 0 && YDistance > 0) {
            arrowAngle += 135;
        }

        // pointing north east
        else if (XDistance > 0 && YDistance < 0) {
            arrowAngle += 45;
        }

        // pointing north west
        else if (XDistance < 0 && YDistance < 0) {
            arrowAngle += 315;
        }

        // pointing south west
        else if (XDistance < 0 && YDistance > 0) {
            arrowAngle += 225;
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