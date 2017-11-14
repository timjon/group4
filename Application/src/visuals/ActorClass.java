package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;


/**
 * An ActorClass wrapper extending DiagramClass. Differs
 * from the latter by images used and canvas position.
 * @author Kosara Golemshinska
 * @version 1.1
 */
public class ActorClass extends DiagramClass {

    // The images retrieved.
    private static Image king1 = new Image("resources/king_cape_up.png");
    private static Image king2 = new Image("resources/king_cape_down.png");

    // The 2 states of the king/actor.
    private Image[] kingStates = {king1, king2};
    // Index used to write the animation state.
    private double kingAnimIndex = 0.0;

    /**
     * Default constructor, calls the super constructor.
     * @param name the class name.
     */
    ActorClass(String name) {
        super(name);
    }

    /**
     * From: Renderable
     * Increments the index to use when accessing
     * the king states array.
     */
    @Override
    public void update() {
        kingAnimIndex += 0.25;
        if (kingAnimIndex >= kingStates.length)
            kingAnimIndex = 0;
    }

    /**
     * Renders the ActorClass on the canvas.
     * @param gc GraphicalContext for a canvas.
     */
    @Override
    public void render(GraphicsContext gc) {
        gc.setFill(Color.TRANSPARENT);

        int x = this.getCoordinates().getX();
        int y = this.getCoordinates().getY();

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
                x + size_ActorClass/4 -this.name.length()*2, // Dynamically determines the x position.
                y - size_ActorClass/2, // Does not need to be dynamic, as all elements scale downwards.
                size + size_ActorClass); // Sets a max width as to not bother the other ActorClasses texts.
    }

}
