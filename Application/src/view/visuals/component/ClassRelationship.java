package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import view.visuals.Renderable;

/**
 * Class for simulating the relationships of the class diagram
 * @version 1.0
 * @author Rashad Kamsheh
 */

public class ClassRelationship implements Renderable {

    //Image for the bridge sprite
    private static Image bridge = new Image("resources/bridgeSprite.png");
    //Image for the arrow at the end of the bridge
    private static Image arrow = new Image("resources/arrow.png");
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
    // initialiser
    public void init(Coordinates fromNode, Coordinates toNode, int size) {
        this.fromNode = fromNode;
        this.toNode = toNode;
        this.size = size;
    }

    @Override
    public void render(GraphicsContext gc) {
        int startingPointX = fromNode.getX();
        int startingPointY = fromNode.getY();
        int endingPointX = toNode.getX();
        //int endingPointY = toNode.getY(); not used atm

        int offset = 0;
        int distance = endingPointX - startingPointX;
        for (int i = 0; i < distance / 15 - 4; i++) {


            if (distance > 0) {
                offset += 15;
            } else {
                offset -= 15;
            }
            gc.drawImage(bridge,
                    startingPointX + offset,
                    startingPointY, 15, 15);
        }
        gc.drawImage(arrow, startingPointX + offset, startingPointY, 15, 15);

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


    // not used for now
    public void setFromNode(Coordinates fromNode) {
        this.fromNode = fromNode;
    }

    // not used for now
    public void setToNode(Coordinates toNode) {
        this.toNode = toNode;
    }
}
