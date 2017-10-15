package visuals;

import javafx.scene.canvas.GraphicsContext;

/**
 * Implementing this assures the class can be rendered through generics.
 */
public interface Renderable {

    // Draws the graphical elements on to the GraphicalContext provided
    void render(GraphicsContext gc);

    // Returns a formatted string containing the Object's identifiers
    String format();

    // Returns the Object's Coordinates.
    Coordinates getCoordinates();
}
