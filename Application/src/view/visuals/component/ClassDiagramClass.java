package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

/**
 * Class for Class diagrams
 * @version 1.0
 * @author Isabelle Törnqvist
 */

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates;
    int size;
    String name;

    private static Image island1 = new Image("resources/Island_with_trees.png");
    private static Image island2 = new Image("resources/Island_with_trees2.png");

    //Animation of Island
    private Image[] islandStates = {island1, island2};
    private double animationIndex = 0.0;

    public ClassDiagramClass(String name) { this.name = name; }

    @Override
    public void render(GraphicsContext gc) {

        gc.setFill(Color.TRANSPARENT);

        Image island = islandStates[(int)animationIndex];
        //Draw class
        gc.drawImage(island,
                this.coordinates.getX() - size/2,
                this.coordinates.getY() - size/2,
                size,
                size*(island.getHeight()/island.getWidth()));

        //Sets the name of the class
        gc.setFill(Color.BLACK);
        gc.fillText(
                this.name,
                this.coordinates.getX()- size/3,
                this.coordinates.getY(),
                island.getWidth());
    }

    /**
     * Changes between the two island, making it "animated"
     */
    @Override
    public void update() {
        animationIndex += 0.25;
        if (animationIndex >= islandStates.length)
            animationIndex = 0;
    }

    /**
     * Does nothing, but is needed
     */
    @Override
    public String format() {
        return null;
    }

    /**
     * @return coordinates
     */
    @Override
    public Coordinates getCoordinates() {
        return coordinates;
    }

    /**
     * @return name
     */
    @Override
    public String getName() {
        String[] s = name.split(":");
        return s[1];
    }

    /**
     * Sets the coordinates and size, used to place the class
     * @param coordinates
     * @param size
     */
    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }
}
