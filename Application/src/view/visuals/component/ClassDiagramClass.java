package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates; // The coordinates of the object.
    int size; // Size of the Object.
    String name; // The class name.

    // The images retrieved.
    private static Image island = new Image("resources/Island_with_trees.png");


    @Override
    public void render(GraphicsContext gc) {

        gc.setFill(Color.TRANSPARENT);

        int x = this.coordinates.getX();
        int y = this.coordinates.getY();

        int size_ClassDiagramClass = size/2;

        placeGraphicCentered(gc, island, size_ClassDiagramClass,0, 0);

    }

    void placeGraphicCentered(GraphicsContext gc, Image img, int size, int offsetX, int offsetY) {
        gc.drawImage(
                img,
                this.coordinates.getX() -size/2 + offsetX,
                this.coordinates.getY() -size/2 + offsetY,
                size,
                size*(img.getHeight()/img.getWidth()));
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
        String[] s = name.split(":");
        return s[1];
    }

    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }
}
