package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates; // The coordinates of the object.
    int size; // Size of the Object.
    String name; // The class name.

    // The images retrieved.
    private static Image island = new Image("resources/iIland_with_trees.png");


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

    @Override
    public String getName() {
        return null;
    }

    @Override
    public void place(Coordinates coordinates, int size) {

    }
}
