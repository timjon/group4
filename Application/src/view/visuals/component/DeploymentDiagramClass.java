package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

/**
 * Class for Deployment diagrams
 * @version 1.0
 * @author Isabelle Törnqvist
 */

public class DeploymentDiagramClass implements  Renderable {

    private Coordinates coordinates; //coordinates
    int size; //size of the allowed node space
    String device; //Name of the device/node/island
    int process_size;
    String process; //Name of the class/process that is mapped on device

    //Images for Islands
    private static Image device1 = new Image("resources/Island_with_trees.png");
    private static Image device2 = new Image("resources/Island_with_trees2.png");

    private static Image processImg = new Image("resources/castle_default.png");

    //Animation of Island
    private Image[] deviceStates = {device1, device2};
    private double animationIndex = 0.0;

    /**
     *
     * @param device setter
     * @param process setter
     */
    public DeploymentDiagramClass(String device, String process) { this.device = device; this.process = process; }

    @Override
    public void render(GraphicsContext gc) {

        gc.setFill(Color.TRANSPARENT);

        Image devices = deviceStates[(int)animationIndex];
        //Draw device
        gc.drawImage(devices,
                this.coordinates.getX() - size/2,
                this.coordinates.getY() - size/2,
                size,
                size*(devices.getHeight()/devices.getWidth()));

        //Sets the name of the device
        gc.setFill(Color.BLACK);
        gc.fillText(
                this.device,
                this.coordinates.getX() + (size/2)/4 -this.device.length()*2,
                this.coordinates.getY() -(size/2),
                size );

        //Draw processes
        process_size = size/4;

        gc.drawImage(processImg,
                this.coordinates.getX() - process_size/2,
                this.coordinates.getY() - process_size/2,
                process_size,
                process_size*(processImg.getHeight()/processImg.getWidth()));

        //Sets the name of the process
        gc.setFill(Color.BLACK);
        gc.fillText(
                this.process,
                this.coordinates.getX() + (process_size/2)/4 -this.process.length()*2,
                this.coordinates.getY() -(process_size/2),
                process_size );
    }

    /**
     * Changes between the two island, making it "animated"
     */
    @Override
    public void update() {
        animationIndex += 0.25;
        if (animationIndex >= deviceStates.length)
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
        String[] s = device.split(":");
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
}