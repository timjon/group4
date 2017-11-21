package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates;
    int size;
    String name;

    private static Image island = new Image("resources/Island_with_trees.png");

    /**
     * @param name the class name.
     */
    ClassDiagramClass(String name) {
        this.name = name;
    }

    @Override
    public void render(GraphicsContext gc) {

        gc.setFill(Color.TRANSPARENT);

        int x = this.coordinates.getX();
        int y = this.coordinates.getY();

        int size_ClassDiagramClass = size/2;

        placeGraphicCentered(gc, island, size_ClassDiagramClass,0, 0);
    }

    /**
     * Places an Image in the center position of the x and y coordinates.
     * @param gc the GraphicalContext where to draw the Image.
     * @param img an Image of what to be displayed.
     * @param size the Size in width, the height is determined based on the scaling of size.
     * @param offsetX the offset in the x position from the placement.
     * @param offsetY the offset in the y position from the placement.
     */
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
        return null;
    }

    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }
}
