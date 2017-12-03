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

    //Image for the bridge sprite which are used to resemble the relationships
    private static Image bridge = new Image("resources/bridgeSprite.png");
    //ImageView that contains the arrow, used ImageView to be able to rotate
    private static ImageView arrow = new ImageView("resources/arrow.png");
    private Coordinates coordinates; //coordinates
    private Coordinates fromNode, toNode;
    private int size; //size of the allowed class space
    private String name; //Name of the relationship

    // constructor
    public ClassRelationship(String name, Coordinates fromNode, Coordinates toNode, int size) {
        this.name = name;
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    // initialiser for rendering the bridge sprites between 2 classes
    public void init(Coordinates fromNode, Coordinates toNode, int size) {
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    @Override
    public void render(GraphicsContext gc) {

        int startingPointX = fromNode.getX(); // Points to the middle of the super class, used for horizontal lines
        int startingPointY = fromNode.getY(); // Points to the middle of the super class, user for vertical lines
        int endingPointX = toNode.getX(); // Points to the middle of the sub class, used for horizontal lines
        int endingPointY = toNode.getY(); // Points to the middle of the sub class, used for horizontal lines
        int XOffset = 0; // Vertical offset
        int YOffset = 0; // Vertical offset
        int offsetIncrementX = 15; // Increments offset
        int offsetIncrementY = 15; // Increments offset
        int XDistance = endingPointX - startingPointX; //if this is negative, ending point it more to the left than starting point on X axis
        int YDistance = endingPointY - startingPointY; // if this is negative, same as above, for Y axis
        int gap = 0; // Used to remove steps from the for loops so that the arrow appears correctly
        int arrowSize = 15; // Initial arrow size is 15 but doubled later for diagonal lines
        int arrowAngle = 0; // used to rotate the arrow to 8 different directions

        // the following 8 if statements are used to rotate the arrow according to vertical and horizontal distance,
        // note: looking for a better solution with less complexity

        // point east
        if (XDistance > 0 && YDistance == 0) {
            arrowAngle += 90;
        }

        // point west
        else if (XDistance < 0 && YDistance == 0) {
            arrowAngle += 270;
        }

        // point north
        else if (XDistance == 0 && YDistance > 0) {
            arrowAngle += 180;
        }

        // point south
        else if (XDistance == 0 && YDistance < 0) {
            arrowAngle += 0;
        }

        // point south east
        else if (XDistance > 0 && YDistance > 0) {
            arrowAngle += 135;
        }

        // point north east
        else if (XDistance > 0 && YDistance < 0) {
            arrowAngle += 45;
        }

        // point north west
        else if (XDistance < 0 && YDistance < 0) {
            arrowAngle += 315;
        }

        // point south west
        else if (XDistance < 0 && YDistance > 0) {
            arrowAngle += 225;
        }

        // negates the operand
        if (XDistance < 0) {
            offsetIncrementX *= (-1);
        }
        // negates the operand
        if (YDistance < 0) {
            offsetIncrementY *= (-1);
        }

        // To solve the issue with all the different cases of linking the classes, we used Bresenham’s Line Algorithm
        // http://csunplugged.org/wp-content/uploads/2014/12/Lines.pdf
        int A, B, P;
        
        // This is used to find out whether to use the Y axis or the X axis as a parameter for the for loop
        // this avoids having 2 loops
        int stepsParameter;

        /// Using the Bresenham’s Line Algorithm on X axis as reference
        if (Math.abs(XDistance) > Math.abs(YDistance)) {
            stepsParameter = Math.abs(XDistance);

            A = 2 * Math.abs(YDistance);
            B = A - 2 * Math.abs((XDistance));
            P = A - Math.abs((XDistance));

            // decreasing the gap to show arrow correctly when it is a horizontal line
            if (YDistance == 0) {
                gap += -2;
            }
            // decreasing the gap to show arrow correctly when it is a horizontal line pointing right
            if (XDistance > 0) {
                gap += -1;
            }
        }
        // Using the Bresenham’s Line Algorithm on the Y axis as reference
        else {
            stepsParameter = Math.abs(YDistance);

            A = 2 * Math.abs(XDistance);
            B = A - 2 * Math.abs((YDistance));
            P = A - Math.abs((YDistance));

            // decreasing the gap to show arrow correctly when it is a vertical line line
            if (XDistance == 0) {
                gap += -1;
            } else {
                gap += -3;
            }
            // decreasing the gap to show arrow correctly when it is a vertical line pointing down
            if (YDistance > 0) {
                gap += -1;
            }
        }

        // This for loop is responsible for drawing the bridge sprites
        for (int i = 0; i < stepsParameter / 15 + gap; i++) {

            // if P is less than 0, draw the next sprite on the same line as the last sprite
            if (P < 0) {

                // increase offset accordingly
                if (Math.abs(XDistance) > Math.abs(YDistance)) {
                    XOffset += offsetIncrementX;
                } else {
                    YOffset += offsetIncrementY;
                }

                gc.drawImage(bridge, startingPointX + XOffset, startingPointY + YOffset, 15, 15);

                P += A; //see Bresenham’s Line Algorithm
            }

            // if P was 0 or greater, draw the next sprite one line higher/lower than the last sprite
            if (P >= 0) {

                // increase offset accordingly
                if (Math.abs(XDistance) > Math.abs(YDistance)) {
                    XOffset += offsetIncrementX;
                } else {
                    YOffset += offsetIncrementY;
                }
                // Draw bridge
                gc.drawImage(bridge, startingPointX + XOffset, startingPointY + YOffset, 15, 15);
                // increase offset accordingly
                if (Math.abs(XDistance) > Math.abs(YDistance)) {
                    YOffset += offsetIncrementY;
                } else {
                    XOffset += offsetIncrementX;
                }
                // Draw second bridge so that diagonal lines look better and wider
                gc.drawImage(bridge, startingPointX + XOffset, startingPointY + YOffset, 15, 15);
                // increase arrow size in the case of diagonal lines because bridges are wider then
                arrowSize = 30;

                P += B; //see Bresenham’s Line Algorithm
            }

        }
        // Rotate arrow image
        arrow.setRotate(arrowAngle);
        // Converts imageView into image
        SnapshotParameters params = new SnapshotParameters();
        params.setFill(Color.TRANSPARENT);
        Image rotatedImage = arrow.snapshot(params, null);
        // Draw an arrow at the end of the bridge
        gc.drawImage(rotatedImage, startingPointX + XOffset, startingPointY + YOffset, arrowSize, arrowSize);

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
