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

    // Game over message.
    private Image game_over = new Image("resources/game_over_screen.png");

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
