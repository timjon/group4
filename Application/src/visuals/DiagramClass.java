package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;

/**
 * A DiagramClass wrapper for coordinates with extra properties that implements Renderable.
 * @author Pontus Laestadius
 * @version 1.0
 */
public class DiagramClass implements Renderable {
    private Coordinates coordinates; // The coordinates of the object.
    int size; // Size of the Object.
    String name; // The class name.

    // The images retrieved.
    private static Image castle1 = new Image("resources/castle_default.png");
    private static Image castle2 = new Image("resources/castle_default2.png");

    static Image platform = new Image("resources/platform_default.png");
    static Image connector = new Image("resources/connector_default.png");
    static Image pillar = new Image("resources/pillar_default.png");

    // Animations

    // The castleStates of the castle.
    private Image[] castleStates = {castle1, castle2};
    private double aniindex = 0.0; // Animation index used for writing the state of the animating sequence.

    /**
     * @param name the class name.
     */
    DiagramClass(String name) {
        this.name = name;
    }

    /**
     * Places the DiagramClass with specific coordinates and size.
     * @param coordinates the position of the DiagramClass.
     * @param size the size for the DiagramClass and it's graphics.
     */
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }

    /**
     * @return the given name and not the class name of this DiagramClass.
     */
    public String getName() {
        String[] s = name.split(":");
        return s[1];
    }

    /**
     * From: Renderable
     * Handles animation processing.
     */
    public void update() {
        aniindex += 0.25;
        if (aniindex >= castleStates.length)
            aniindex = 0;
    }

    /**
     * From: Renderable
     * @return nothing, as it's not in use.
     */
    public String format() {
        return null;
    }

    /**
     * @return the DiagramClass coordinates
     */
    public Coordinates getCoordinates() {
        return coordinates;
    }

    /**
     * Renders the DiagramClass on the canvas
     * @param gc GraphicalContext for a canvas.
     */
    public void render(GraphicsContext gc) {
        gc.setFill(Color.TRANSPARENT);

        int x = this.coordinates.getX();
        int y = this.coordinates.getY();

        int size_DiagramClass    = size/2;
        int size_lifeline = size_DiagramClass/4;
        int size_platform = size_DiagramClass + size_DiagramClass/5;

        // Draws the pillar.
            double scale = (pillar.getHeight()/pillar.getWidth()); // Used for scaling the pillar to the size of the DiagramClass.
            int pillar_height = (int) (scale*size_lifeline);

        // Draws the pillar from the height of the DiagramClass until the end of the canvas.
            for (int i = 0; i < gc.getCanvas().getHeight()-y; i+=pillar_height) // Iterates over scale to the end of the canvas.
            /* Draws the pillar: The x position needs to be the center of the DiagramClass minus half of the width of the pillar
             * we are drawing, This gives us (x +size_DiagramClass/2 -size_lifeline/2). The y only has to be provided a increased
             * starting position as to start from below the DiagramClass drawn, and then in each iteration add the scale to i to
             * create a solid pillar.*/
                gc.drawImage(pillar, x -size_lifeline/2, y+i, size_lifeline, pillar_height);

        // Draws the connector.
        placeGraphicCentered(gc, connector, size_lifeline*2, 0, size_DiagramClass -size_lifeline/2);
        // Draws the platform.
        placeGraphicCentered(gc, platform, size_platform, 0, size_platform/2);
        // Draws the DiagramClass.

        Image castle_state = castleStates[(int)aniindex];
        placeGraphicCentered(gc, castle_state, size_DiagramClass,0, 0);

        // Draws the name of the DiagramClass.
            gc.setFill(Color.BLACK); // Selects BLACK to be the color of the text.
            gc.fillText(
                    this.name, // Sets the text to be the name of the DiagramClass.
                    x + size_DiagramClass/4 -this.name.length()*2, // Dynamically determines the x position.
                    y -size_DiagramClass/2, // Does not need to be dynamic, as all elements scale downwards.
                    size +size_DiagramClass); // Sets a max width as to not bother the other DiagramClasses texts.
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
}
