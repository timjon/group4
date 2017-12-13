package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

/**
 * Class for Class diagrams
 * @version 1.2
 * @author Isabelle Törnqvist
 * Collaborators: Pontus Laestadius
 */

public class ClassDiagramClass implements Renderable {

    private Coordinates coordinates; //coordinates
    int size; //size of the allowed class space
    String name; //Name of the class
    private boolean highlight = false; // State of highlighting

    //Images for classes
    private static Image class1 = new Image("resources/castle_default.png");
    private static Image class2 = new Image("resources/castle_default2.png");

    //Animation of castle
    private Image[] classStates = {class1, class2};
    private double animationIndex = 0.0;

    /**
     * @param name class name setter
     */
    public ClassDiagramClass(String name) { this.name = name; }

    public ClassDiagramClass() { }

    @Override
    public void render(GraphicsContext gc) {

        // If this class in highlighted.
        if (highlight) {
            gc.setFill(Color.YELLOW);
            gc.fillRoundRect(this.coordinates.getX() -size/2 -5, this.coordinates.getY() -size/2 -5, size +10, size +10,
                    size/3, size/3);
        }

        gc.setFill(Color.TRANSPARENT);

        Image classes = classStates[(int)animationIndex];
        //Draw class
        gc.drawImage(classes,
                this.coordinates.getX() - size/2,
                this.coordinates.getY() - size/2,
                size,
                size*(classes.getHeight()/classes.getWidth()));

        //Sets the name of the class
        gc.setFill(Color.BLACK);
        gc.fillText(
                this.name,
                this.coordinates.getX() + (size/2)/4 -this.name.length()*2,
                this.coordinates.getY() -(size/2),
                size );
    }

    /**
     * Changes between the two castles, making it "animated"
     */
    @Override
    public void update() {
        animationIndex += 0.25;
        if (animationIndex >= classStates.length)
            animationIndex = 0;
    }

    /**
     * Does nothing, but is needed
     */
    @Override
    public String format() {
        return null;
    }

    /**
     * @return coordinates
     */
    @Override
    public Coordinates getCoordinates() {
        return coordinates;
    }

    /**
     * @return name
     */
    @Override
    public String getName() {
        String[] s = name.split(":");
        return s[1];
    }

    /**
     * Sets the coordinates and size, used to place the class
     * @param coordinates
     * @param size
     */
    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }

    /**
     * Sets the highlight state of this class.
     * @param state false to disable true to enable.
     */
    public void highlight(boolean state) {
        this.highlight = state;
    }
}
