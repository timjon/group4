package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;

/**
 * A class wrapper for coordinates with extra properties that implements Renderable.
 * @author Pontus Laestadius
 */
public class Class implements Renderable {
    private Coordinates coordinates;
    private int size;
    private String name;

    public Class(Coordinates coordinates, int size, String name) {
        this.name = name;
        this.size = size;
        this.coordinates = coordinates;
    }

    public Class(String name) {
        this.name = name;
    }

    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
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
    public void render(GraphicsContext gc) {
        Image image = new Image("resources/castle.png");
        int x = this.coordinates.getX();
        int y = this.coordinates.getY();
        gc.drawImage(image, x, y, size/2, size/2);
        int len = this.name.length();

        int pos_x = x + size/4 - len*2;

        gc.fillText(this.name, pos_x, y -15);
    }

    public String getName() {
        return name;
    }
}
