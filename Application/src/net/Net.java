package net;

import visuals.ExecutionLog;
import javafx.application.Platform;

import java.util.PriorityQueue;
import java.util.Queue;

/**
 * @author Pontus Laestadius
 * @version 1.0
 */
public class Net implements Runnable {

    // A single static connection. Handled with a level of abstraction with this class.
    private static Server_connection singletonServer_connection = null;

    // A message queue for sending messages to the server.
    private static Queue<String> queue = new PriorityQueue<>();

    @Override
    public void run() {
        inner_loop();
    }

    /**
     * Only allow a single network thread and it must be run on an FXApplicationThread.
     * Starts a new Net thread.
     */
    public static void init() {
        // Only run if it's on an FX thread, meaning it will become a worker / background thread.
        // And if there is no existing connection.
        if (singletonServer_connection == null && Platform.isFxApplicationThread())
            (new Thread(new Net())).start();
    }

    /**
     * Pushes a string to queue which will be processed in the order they are received.
     * @param string to be sent to the server.
     */
    public static void push(String string) {
        queue.add(string);
    }

    /**
     * Runs a private inner_loop for reading and writing to the server safely.
     */
    private void inner_loop() {

        // Create a new server connection.
        singletonServer_connection = new Server_connection();

        // Handles connecting and reconnecting, Should always run, because if this loop breaks no connection exists.
        while (true) {

            // Opens the connection to the server.
            singletonServer_connection.openConnection();

            // Retrieves the instance of the execution log.
            ExecutionLog executionLog = ExecutionLog.getInstance();

            // Loop while it is connected to the server.
            while (singletonServer_connection.checkConnection()) {


                // This makes the application work. It may not be touched.
                // Without this 1 ms delay the message are not retrieved from the server. For an unknown reason.
                try {
                    Thread.sleep(1);
                } catch (InterruptedException e) {
                    System.err.println(e);
                }

                // Only do anything if there is a message to send.
                if (queue.isEmpty()) continue;

                // Sends the first message in queue.
                singletonServer_connection.sendMessage(queue.poll());

                // Blocks until a message is received.
                String result = singletonServer_connection.receiveMessage();

                // Run the decode on the a Platform thread.
                Platform.runLater(() -> {

                    // Create a new decode object with the message received.
                    Decode decode = new Decode(result);

                    // Execute the decoding process.
                    decode.execute();
                });
            }

            // Clears the ExecutionLog and prints no connection, if you are disconnected.
            Platform.runLater(() -> {

                // Clear the execution log
                executionLog.clear();

                // Write to the execution log that it couldn't connect.
                executionLog.fwd("No connection established");
            });

            // Sleep for for a longer duration before attempting to reconnect. If
            // Occurs if you were never connected, or if you disconnected.
            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                System.err.println(e);
            }

        }

    }

}
