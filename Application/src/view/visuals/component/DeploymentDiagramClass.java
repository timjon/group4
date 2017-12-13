package view.visuals.component;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import view.visuals.Renderable;

import java.util.ArrayList;

/**
 * Class for Deployment diagrams
 * @version 1.0
 * @author Isabelle Törnqvist
 */

public class DeploymentDiagramClass extends ClassDiagramClass {

    private Coordinates coordinates; //coordinates
    int size; //size of the allowed node space
    String device; //Name of the device/node/island
    int process_size;
    private ArrayList<String> processes = new ArrayList<>();    //Arraylist of processes mapped to device

    //Images for Islands/devices
    private static Image device1 = new Image("resources/Island_with_trees.png");
    private static Image device2 = new Image("resources/Island_with_trees2.png");
    //Images for castles/processes
    private static Image processImg = new Image("resources/castle_default.png");
    private static Image processImg2 = new Image("resources/castle_default2.png");

    //Animation of Island
    private Image[] deviceStates = {device1, device2};
    //Animation of castles
    private Image[] processStates = {processImg, processImg2};
    //Used in animation of castles and islands
    private double animationIndex = 0.0;

    /**
     * @param device setter
     */
    public DeploymentDiagramClass(String device) {
        this.device = device;
    }

    //Maps process to device
    public void addProcess(String process) {
        processes.add(process);
    }

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

        //Allowed size of the image for the process
        process_size = size/4;

        //creates an offset, better placement of process on device
        int offset = -process_size/2;
        //Animation of castle
        Image processImgs = processStates[(int)animationIndex];

        //For each process mapped to this device
        for(String process : processes){
            //draws
            gc.drawImage(processImgs,
                    (this.coordinates.getX() - process_size/2) + offset,
                    this.coordinates.getY() - process_size/2,
                    process_size,
                    process_size*(processImgs.getHeight()/processImgs.getWidth()));

            //Sets the name of the process
            gc.setFill(Color.BLACK);
            gc.fillText(
                    process,
                    (this.coordinates.getX() + (process_size/2)/4 - process.length()*2) + offset,
                    this.coordinates.getY() -(process_size/2),
                    process_size );

            offset += process_size + 5;
        }
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
     * @return device
     */
    @Override
    public String getName() {
        return device;
    }

    /**
     * Sets the coordinates and size, used to place the device
     * @param coordinates
     * @param size
     */
    @Override
    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }
}