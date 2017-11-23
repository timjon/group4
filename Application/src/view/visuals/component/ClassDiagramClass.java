package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

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
        animationIndex += 0.25;
        if (animationIndex >= islandStates.length)
            animationIndex = 0;
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
