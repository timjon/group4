package view.visuals;

import javafx.scene.canvas.GraphicsContext;
import view.visuals.component.Coordinates;

/**
 * Implementing this assures the class can be rendered through generics.
 * @author Pontus Laestadius
 * @version 1.0
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

    // Returns the name for this object.
    String getName();

    // Places the object at the specified coordinates.
	void place(Coordinates coordinates, int size);
}
