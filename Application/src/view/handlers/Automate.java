package view.handlers;

import controller.network.Net;
import model.Menu;
import view.DiagramView;
import view.ExecutionLog;
import view.visuals.component.Message;

/**
 * Handles automating the Sequence Diagram execution.
 * @version 1.1
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
                    Menu.getInstance().identifyState();
                    break;
                }

                // Get the message to check for it's status.
                Message message;
                try {
                    // Get the last message.
                    message = DiagramView.getDiagramViewInView().getDraw().getLastMessage();

                    // If the last message is done animating.
                    if (!message.isAnimating()) {
                        Net.changeMessage("next_message");
                    }
                }
                // If there are no messages
                catch (ArrayIndexOutOfBoundsException ex) {
                    Net.changeMessage("next_message");
                }
            }

            catch (Exception ex) {
                ex.printStackTrace();
            }
        }

    }

}
