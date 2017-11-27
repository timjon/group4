package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import view.visuals.Renderable;


/**
 * Holds the graphics for the game over message.
 * @author Kosara Golemshinska
 * @version 0.1
 */
public class GameOver implements Renderable {

    private Coordinates coordinates; // The coordinates of the object.
    int size; // Size of the Object.
    private Image game_over = new Image("resources/game_over_screen.png");  // Game over message.

    /**
     * Default constructor.
     */
    public GameOver() {

    }

    /**
     * Renders the game over message on the canvas.
     * @param gc graphics context
     */
    @Override
    public void render(GraphicsContext gc) {
        int width = (int)gc.getCanvas().getWidth();
        int height = (int)gc.getCanvas().getHeight();
        int size2 = (int)(size*(game_over.getHeight()/game_over.getWidth()));
        gc.drawImage(game_over, (width-size)/2, height/2.5-size2/2, size,
                size2);
    }

    /**
     * Not applicable as it's not animated.
     */
    @Override
    public void update() {

    }

    /**
     * Not applicable to the current class
     * @return null as it's not in use
     */
    @Override
    public String format() {
        return null;
    }

    /**
     * Returns the current coordinates of the game over message.
     * @return the current coordinates
     */
    @Override
    public Coordinates getCoordinates() {
        return coordinates;
    }

    /**
     * Not applicable to this class.
     * @return null as it's not in use
     */
    @Override
    public String getName() {
        return null;
    }

    /**
     * Sets the coordinates and size of the object to the given ones.
     * @param coordinates the current coordinates of the object
     * @param size the current size of the object
     */
    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }
}
