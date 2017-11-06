package visuals.handlers;

import model.Menu;
import net.Net;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;
import visuals.Message;

/**
 * Handles automating the Sequence Diagram execution.
 * @version 0.1
 * @author Pontus Laestadius
 */
public class Automate extends Thread {

    // The thread the animations are updated from.
    private static Thread singletonAutomateThread;

    /**
     * Stops the Animation thread at the end of the next iteration.
     */
    public static void cancel() {
        // Sets this thread to null.
        singletonAutomateThread = null;
    }

    @Override
    public void run() {

        // Only allow a single Animation thread.
        if (singletonAutomateThread == null) {

            // If none exists, reserve it for this thread.
            singletonAutomateThread = this;

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
        while (singletonAutomateThread != null) {
            try {
                Thread.sleep(250);

                Message message = DiagramView.getDiagramViewInView().getDraw().getLastMessage();

                // If the last message is done animating.
                if (!message.isKeepAnimating()) {
                    Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", next_message}");
                }

                if (ExecutionLog.getInstance().isFinished()) {
                    cancel();
                    Menu.pause();
                }

                // If there is no message. An exception is caught.
            } catch (Exception ex) {
                Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", next_message}");
            }
        }

    }

}
