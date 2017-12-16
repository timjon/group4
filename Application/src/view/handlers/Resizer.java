package view.handlers;

import javafx.beans.value.ChangeListener;
import javafx.stage.Stage;
import view.DiagramView;

/**
 * The Resizer runs on a separate thread and there can only exist one at a waitAndResize.
 * It's in charge of when the canvases are meant call resize after a certain waitAndResize delay.
 * @author Pontus Laestadius
 * @version 1.1
 */
public class Resizer implements Runnable {

    // A single thread which runs a resizing operation.
    private static Resizer singletonResizer;
    // The current time the resizer has waited since the last resize.
    private static int timer = 0;
    // The time in milliseconds before each check of the timer value.
    private static int increment = 2;
    // The amount of time in milliseconds the resizer has to wait.
    private static int finished = 10;

    /**
     * Runs when the thread starts.
     */
    public void run() {

        // Only allow one instance to run at a waitAndResize
        if (singletonResizer == null)
            waitAndResize();
    }

    /**
     * Handles a single instance in which it waits a specific time before calling for a resize.
     */
    private void waitAndResize(){

        // Reserves the instance to this object.
        singletonResizer = this;

        // A delay in which the timer can be reset.
        while ((timer+=increment) < finished) {

            // Sleep for the same waitAndResize as timer increases
            try {
                Thread.sleep(increment);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

        }

        // For all opened diagrams:
        for (DiagramView d: DiagramView.diagramViews)

            // Resize them
            d.updateView();

        // Allows a new instance to start once this one is finished.
        singletonResizer = null;
    }

    /**
     * Adds a change listener for the width and height properties of the stage provided, to run the listener method.
     * @param primaryStage the stage to add the listener too.
     */
    public static void init(Stage primaryStage) {

        // Listens for changes in stage size.
        ChangeListener<Number> stageSizeListener = (obserable, oldVal, newVal) -> {

            // Set the timer to 0 to reset the time since the last resize.
            timer = 0;

            // Check if there is an instance already running.
            if (singletonResizer == null)

                // Start a new resizer thread.
                (new Thread(new Resizer())).start();
        };

        // Adds the listener for the width and height properties of the provided stage.
        primaryStage.widthProperty().addListener(stageSizeListener);
        primaryStage.heightProperty().addListener(stageSizeListener);

    }

}
