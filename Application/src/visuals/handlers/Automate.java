package visuals.handlers;

import model.Menu;
import net.Net;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;
import visuals.Message;

/**
 * Handles automating the Sequence Diagram execution.
 * @version 1.0
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

    /**
     * @return if the thread is occupied or not by a running process.
     */
    public static boolean running() {
        return singletonAutomateThread != null;
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
                Thread.sleep(500);

                // Check if we are done.
                if (ExecutionLog.getInstance().isFinished()) {
                    cancel();
                    Menu.pause();
                    break;
                }

                // Get the message to check for it's status.
                Message message;
                try {
                    // Get the last message.
                    message = DiagramView.getDiagramViewInView().getDraw().getLastMessage();

                    // If the last message is done animating.
                    if (!message.isKeepAnimating()) {
                        Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", next_message}");
                    }
                }
                // If there are no messages
                catch (ArrayIndexOutOfBoundsException ex) {
                    // send a request for a message.
                    Net.push("{" + DiagramView.getDiagramViewInView().getTab().getId() + ", next_message}");
                }
            }

            catch (Exception ex) {
                ex.printStackTrace();
            }
        }

    }

}
