package visuals;

/**
 * Holds x and y positions
 * @author Pontus Laestadius
 */
public class Coordinates {
    private int x;
    private int y;

    Coordinates(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }
}