package visuals;

import javafx.scene.canvas.GraphicsContext;

/**
 * Implementing this assures the class can be rendered through generics.
 */
public interface Renderable {

    // Draws the graphical elements on to the GraphicalContext provided
    void render(GraphicsContext gc);

    // Updates the animation sequence
    void update();

    // Returns a formatted string containing the identifiers
    String format();

    // Returns the Object's Coordinates.
    Coordinates getCoordinates();
}