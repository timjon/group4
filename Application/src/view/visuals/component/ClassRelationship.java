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
 * @version 1.1
 */

public class ClassRelationship implements Renderable {


    //Image for the road sprite which are used to resemble the class diagram relationships
    private static Image road = new Image("resources/roadSprite.png");
    //ImageView that contains the arrow, used ImageView to be able to rotate
    private static ImageView arrow = new ImageView("resources/road-arrow.png");
    private Coordinates coordinates; //coordinates
    private Coordinates fromNode, toNode;
    private int size; //size of the allowed class space

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
        bresenhamsParametersArray[0] = Math.abs(XDistance); //stepsParameter
        bresenhamsParametersArray[1] = 2 * Math.abs(YDistance); // A
        bresenhamsParametersArray[2] = bresenhamsParametersArray[1] - 2 * Math.abs((XDistance)); // B
        bresenhamsParametersArray[3] = bresenhamsParametersArray[1] - Math.abs((XDistance)); // P

        return bresenhamsParametersArray;
    }

    @Override
    public void render(GraphicsContext gc) {

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
        // we need 2 offset increment integers for the case where we have to negate the operand of only one of them
        int offsetIncrementX = 15; // Increments offset of the X axis offset
        int offsetIncrementY = 15; // Increments offset of the Y axis offset

        // negates the operand
        if (XDistance < 0) {
            offsetIncrementX *= (-1);
        }
        // negates the operand
        if (YDistance < 0) {
            offsetIncrementY *= (-1);
        }

        // This is used to find out whether to use the Y axis or the X axis as a parameter for the for loop
        int stepsParameter;
        int A, B, P; // Variables used for Bresenham’s Line Algorithm

        /// Using the Bresenham’s Line Algorithm on X axis as reference
        if (Math.abs(XDistance) > Math.abs(YDistance)) {
            int[] params = initBresenhamsParameters(XDistance, YDistance);
            stepsParameter = params[0];
            A = params[1];
            B = params[2];
            P = params[3];
        }
        // Using the Bresenham’s Line Algorithm on the Y axis as reference by swapping the parameters
        else {
            int[] params;
            //Parameters swapped
            params = initBresenhamsParameters(YDistance, XDistance);
            stepsParameter = params[0];
            A = params[1];
            B = params[2];
            P = params[3];
        }

        int arrowSize = 15; // Initial arrow size is 15 but doubled later for diagonal lines

        // This for loop is responsible for drawing the road sprites
        for (int i = 0; i < stepsParameter / 20; i++) {

            // if P is less than 0, draw the next sprite on the same line as the last sprite
            if (P < 0) {

                // increase offset accordingly
                if (Math.abs(XDistance) > Math.abs(YDistance)) {
                    XOffset += offsetIncrementX;
                } else {
                    YOffset += offsetIncrementY;
                }

                // Draw road sprites
                gc.drawImage(road, startingPointX + XOffset, startingPointY + YOffset, size, size);

                P += A; //see Bresenham’s Line Algorithm
            }

            // if P was 0 or greater, draw the next sprite one line higher/lower than the last sprite
            if (P >= 0) {

                // increase both offsets
                XOffset += offsetIncrementX;
                YOffset += offsetIncrementY;
                // Draw road sprites
                System.out.println(size);
                gc.drawImage(road, startingPointX + XOffset, startingPointY + YOffset, size, size);

                // increase arrow size in the case of diagonal lines
                arrowSize = 25;

                P += B; //see Bresenham’s Line Algorithm
            }
        }

        // Rotate inheritance arrow
        arrow.setRotate(getArrowAngle(XDistance, YDistance));
        // Converts imageView into image
        SnapshotParameters params = new SnapshotParameters();
        params.setFill(Color.TRANSPARENT);
        Image rotatedImage = arrow.snapshot(params, null);
        // Draw an arrow at the end of the road
        gc.drawImage(rotatedImage, startingPointX + XOffset, startingPointY + YOffset, arrowSize, arrowSize);
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