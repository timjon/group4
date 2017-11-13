package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;

/**
 * An ActorClass wrapper modeled after DiagramClass.
 * @author Pontus Laestadius
 * collaborator Kosara Golemshinska
 * @version 1.0
 */
public class ActorClass implements Renderable {

	private Coordinates coordinates; // The coordinates of the object.
    private int size; // Size of the Object.
    private String name; // The class name.

    // The images retrieved.
    private static Image king1 = new Image("resources/king_cape_up.png");
    private static Image king2 = new Image("resources/king_cape_down.png");

    private static Image platform = new Image("resources/platform_default.png");
    private static Image connector = new Image("resources/connector_default.png");
    private static Image pillar = new Image("resources/pillar_default.png");

    // Animations

    // The 2 states of the king/actor.
    private Image[] kingStates = {king1, king2};
    // Index used to write the animation state.
    private double kingAnimIndex = 0.0;

    /**
     * @param name the class name.
     */
    ActorClass(String name) {
       this.name = name;
   }

    /**
     * Places the ActorClass with specific coordinates and size.
     * @param coordinates the position of the ActorClass.
     * @param size the size for the ActorClass and it's graphics.
     */
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }

    /**
     * @return the given name and not the class name of this ActorClass.
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
        kingAnimIndex += 0.25;
        if (kingAnimIndex >= kingStates.length)
        	kingAnimIndex = 0;
    }

    /**
     * From: Renderable
     * @return nothing, as it's not in use.
     */
    public String format() {
        return null;
    }

    /**
     * @return the ActorClass coordinates
     */
    public Coordinates getCoordinates() {
        return coordinates;
    }

    /**
     * Renders the ActorClass on the canvas
     * @param gc GraphicalContext for a canvas.
     */
    public void render(GraphicsContext gc) {
        gc.setFill(Color.TRANSPARENT);

        int x = this.coordinates.getX();
        int y = this.coordinates.getY();

        int size_ActorClass    = size/2;
        int size_lifeline = size_ActorClass/4;
        int size_platform = size_ActorClass + size_ActorClass/5;

        // Draws the pillar.
            double scale = (pillar.getHeight()/pillar.getWidth()); // Used for scaling the pillar to the size of the ActorClass.
            int pillar_height = (int) (scale*size_lifeline);

        // Draws the pillar from the height of the ActorClass until the end of the canvas.
            for (int i = 0; i < gc.getCanvas().getHeight()-y; i+=pillar_height) // Iterates over scale to the end of the canvas.
            /* Draws the pillar: The x position needs to be the center of the ActorClass minus half of the width of the pillar
             * we are drawing, This gives us (x +size_ActorClass/2 -size_lifeline/2). The y only has to be provided a increased
             * starting position as to start from below the ActorClass drawn, and then in each iteration add the scale to i to
             * create a solid pillar.*/
                gc.drawImage(pillar, x -size_lifeline/2, y+i, size_lifeline, pillar_height);

        // Draws the connector.
        placeGraphicCentered(gc, connector, size_lifeline*2, 0, size_ActorClass -size_lifeline/2);
        // Draws the platform.
        placeGraphicCentered(gc, platform, size_platform, 0, size_platform/2);
        // Draws the ActorClass.
        
        Image king_state = kingStates[(int)kingAnimIndex];
        placeGraphicCentered(gc, king_state, size_ActorClass/2,-size_ActorClass/8, -size_ActorClass/6);

        // Draws the name of the ActorClass.
            gc.setFill(Color.BLACK); // Selects BLACK to be the color of the text.
            gc.fillText(
                    this.name, // Sets the text to be the name of the ActorClass.
                    x + size_ActorClass/4,// -this.name.length()*2, // Dynamically determines the x position.
                    y -size_ActorClass/2, // Does not need to be dynamic, as all elements scale downwards.
                    size +size_ActorClass); // Sets a max width as to not bother the other ActorClasses texts.
    }

    /**
     * Places an Image in the center position of the x and y coordinates.
     * @param gc the GraphicalContext where to draw the Image.
     * @param img an Image of what to be displayed.
     * @param size the Size in width, the height is determined based on the scaling of size.
     * @param offsetX the offset in the x position from the placement.
     * @param offsetY the offset in the y position from the placement.
     */
    private void placeGraphicCentered(GraphicsContext gc, Image img, int size, int offsetX, int offsetY) {
        gc.drawImage(
                img,
                this.coordinates.getX() -size/2 + offsetX,
                this.coordinates.getY() -size/2 + offsetY,
                size,
                size*(img.getHeight()/img.getWidth()));
    }
}