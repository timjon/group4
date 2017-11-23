package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates;
    int size;
    String name;

    private static Image island = new Image("resources/Island_with_trees.png");

    public ClassDiagramClass(String name) { this.name = name; }

    @Override
    public void render(GraphicsContext gc) {

        gc.setFill(Color.TRANSPARENT);
        int size_DiagramClass = size/2;

        gc.drawImage(island,
                this.coordinates.getX() - size/2,
                this.coordinates.getY() - size/2,
                size,
                size*(island.getHeight()/island.getWidth()));


        gc.setFill(Color.BEIGE);
        gc.fillText(
                this.name,
                this.coordinates.getX(),
                this.coordinates.getY(),
                island.getWidth());



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
