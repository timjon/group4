package view.visuals.component;

/**
 * Holds x and y positions
 * @author Pontus Laestadius
 * @version 1.0
 */
public class Coordinates {
    private int x;
    private int y;

    /**
     * Initializes the object with all fields.
     * @param x the position relative to the horizontal alignment.
     * @param y the position relative to the vertical alignment.
     */
    public Coordinates(int x, int y) {
        this.x = x;
        this.y = y;
    }

    /**
     * @return the x coordinate.
     */
    int getX() {
        return x;
    }

    /**
     * @return the y coordinate.
     */
    int getY() {
        return y;
    }
}