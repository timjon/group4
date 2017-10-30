package visuals.handlers;

import visuals.DiagramView;
import visuals.Draw;

/**
 * Handles animation requests to the Draw object.
 * @version 1.0
 * @author Pontus Laestadius
 */
public class Animation extends Thread {
    private static Thread singletonAnimationThread;

    @Override
    public void run() {
        if (singletonAnimationThread == null) { // Only allow a single Animation thread.
            singletonAnimationThread = this; // If none exists, reserve it for this thread.
            loop(); // Animation handling.
        }
    }

    /**
     * Lives for the duration of the thread,
     * Updates and redraws the canvas.
     */
    private void loop() {
        while (singletonAnimationThread != null) { // Makes terminating the thread externally possible.
            try { // Sleeps for the time in between updates.
                Thread.sleep(1000/2); // 5 frames per second.
            } catch (InterruptedException e) {
                System.err.println(e.toString());
            }
            // Retrieves the draw that is in view and updates and redraws it.
            DiagramView dv = DiagramView.getDiagramViewInView();
            if (dv == null) continue; // If a View is not established. Skip.
            Draw draw = dv.getDraw(); // Get the Draw object from the view.
            draw.update(); // Updates the states of the Renderable objects.
            draw.redraw(); // Redraws their graphics on the canvas.
        }
    }
}
