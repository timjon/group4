package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import view.visuals.Renderable;

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates; // The coordinates of the object.
    int size; // Size of the Object.
    String name; // The class name.

    // The images retrieved.
    private static Image island = new Image("resources/Island_with_trees.png");


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
        return coordinates;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }
}
