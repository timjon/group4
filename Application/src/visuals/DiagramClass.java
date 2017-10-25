package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;

/**
 * A DiagramClass wrapper for coordinates with extra properties that implements Renderable.
 * @author Pontus Laestadius
 */
public class DiagramClass implements Renderable {
    private Coordinates coordinates;
    private int size;
    private String name;

    // Animations
    private int aniindex = 0; // Animation index used for writing the state of the animating sequence.
    private Coordinates[] anicontent; // The sequence that the object follows when animated.

    private static Image castle = new Image("resources/castle.png");
    private static Image platform = new Image("resources/platform.png");
    private static Image pillar = new Image("resources/pillar.png");

    DiagramClass(String name) {
        this.name = name;
        anicontent = new Coordinates[1];
        anicontent[0] = new Coordinates(0,0);
    }

    /**
     * Places the DiagramClass with specific coordinates and size.
     * @param coordinates the position of the DiagramClass.
     * @param size the size for the DiagramClass and it's graphics.
     */
    void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }

    /**
     * From: Renderable
     * Handles animation processing.
     */
    public void update() {
        aniindex = aniindex == anicontent.length-1 ? 0: aniindex+1;
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
        int size_platform = size_DiagramClass + size_DiagramClass/2;

        // Draws the pillar.
            int scale = (int) (pillar.getHeight()/size_lifeline); // Used for scaling the pillar to the size of the DiagramClass.

            // Draws the pillar from the height of the DiagramClass until the end of the canvas.
            for (int i = 0; i < gc.getCanvas().getHeight()-y; i+=scale) // Iterates over scale to the end of the canvas.
            /* Draws the pillar: The x position needs to be the center of the DiagramClass minus half of the width of the pillar
             * we are drawing, This gives us (x +size_DiagramClass/2 -size_lifeline/2). The y only has to be provided a increased
             * starting position as to start from below the DiagramClass drawn, and then in each iteration add the scale to i to
             * create a solid pillar.*/
                gc.drawImage(pillar, x +size_DiagramClass/2 -size_lifeline/2,y + size/4 +i, size_lifeline, scale);

        // Draws the platform.
            gc.drawImage(platform,x -size/8,y +size/2 -size/8, size_platform,size/6);

        // Draws the DiagramClass.
            gc.drawImage(castle, x, y, size/2, size/2);

        // Draws the name of the DiagramClass.
            gc.setFill(Color.BLACK); // Selects BLACK to be the color of the text.
            gc.fillText(
                    this.name, // Sets the text to be the name of the DiagramClass.
                    x + size_DiagramClass/2 -this.name.length()*2, // Dynamically determines the x position.
                    y -15, // Does not need to be dynamic, as all elements scale downwards.
                    size +size_DiagramClass); // Sets a max width as to not bother the other DiagramClasses texts.
    }
}
