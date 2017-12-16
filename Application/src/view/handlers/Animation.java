package view.handlers;

import javafx.application.Platform;
import view.DiagramView;
import view.visuals.Draw;

/**
 * Handles animation requests to the Draw object.
 * @version 1.1
 * @author Pontus Laestadius
 */
public class Animation extends Thread {

    // The thread the animations are updated from.
    private static Thread singletonAnimationThread;

    // Variable used to pinpoint the update time so it's always the same.
    private long timeSinceLastUpdate = 0;

    // Defaults framesPerSecond the application updates in.
    private static int framesPerSecond = 12;

    /**
     * Stops the Animation thread at the end of the next iteration.
     */
    public static void cancel() {
        // Sets this thread to null.
        singletonAnimationThread = null;
    }

    /**
     * @param framesPerSecond to animate the application in.
     */
    public static void setFramesPerSecond(int framesPerSecond) {
        Animation.framesPerSecond = framesPerSecond;
    }

    @Override
    public void run() {

        // Only allow a single Animation thread.
        if (singletonAnimationThread == null) {

            // If none exists, reserve it for this thread.
            singletonAnimationThread = this;

            // Animation handling.
            loop();
        }

    }

    /**
     * Lives for the duration of the thread,
     * Updates and redraws the canvas.
     */
    private void loop() {

        // Makes terminating the thread externally possible.
        while (singletonAnimationThread != null) {

            // Sleeps for the time in between updates.
            try {

                // Sleep for the time in between frames, minus the time the last frame took to render.
                // To keep the frame times consistent.
                Thread.sleep(1000/framesPerSecond -timeSinceLastUpdate);

                // If the thread gets interrupted.
            } catch (InterruptedException e) {
                e.printStackTrace();

                // Try again.
                continue;
            }

            // Gets the time before animating a frame.
            long currentTimeBeforeThisAnimationFrame = System.currentTimeMillis();

            // Catches if there is not view.
            try {
                // Retrieves the draw that is in view and updates and redraws it.

                // Get the Draw object from the view.
                Draw draw = DiagramView.getDiagramViewInView().getDraw();

                // Updates the states of the Renderable objects.
                draw.update();

                // Redraws their graphics on the canvas.
                Platform.runLater(() -> {
                    draw.redraw();
                });

                // If there is no view.
            } catch (IllegalStateException ex) {
                ex.printStackTrace();

                // Try again.
                continue;
            }

            // Sets the time since last update to be the render time.
            // And if it's bigger than the delay time. Set to 0.
            timeSinceLastUpdate =
                    (System.currentTimeMillis() -currentTimeBeforeThisAnimationFrame > 1000/framesPerSecond ?
                    0 : System.currentTimeMillis() -currentTimeBeforeThisAnimationFrame);
        }

    }

}
