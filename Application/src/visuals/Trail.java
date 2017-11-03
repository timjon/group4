package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;

/**
 * Shows the trails of the messages being sent.
 * @author Isabelle Törnqvist
 * @version 1.0
 */

public class Trail implements Renderable {
    private Coordinates coordinates , node1, node2;
    private int nodeSend, nodeReceive;
    private int class_size;

    private static Image cloud = new Image("resources/Cloud1.png");

    public Trail(Coordinates node1, Coordinates node2, int nodeSend, int nodeReceive, int size){
        this.node1 = node1;
        this.node2 = node2;
        this.nodeSend = nodeSend;
        this.nodeReceive = nodeReceive;
        this.class_size = size;
    }

    @Override
    public void render(GraphicsContext gc) {

    }

    @Override
    public void update() {

    }

    @Override
    public String format() {
        return null;
    }

    @Override
    public Coordinates getCoordinates() {
        return null;
    }
}

