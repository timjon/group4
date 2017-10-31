package net;

import visuals.DiagramView;
import visuals.ExecutionLog;
import javafx.application.Platform;
import visuals.handlers.Animation;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

/**
 * @author Pontus Laestadius
 * @version 1.0
 */
public class Net implements Runnable {
    private static Server_connection con = null;
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
        if (con == null && Platform.isFxApplicationThread())
            (new Thread(new Net())).start();
    }

    /**
     * Pushes a string to queue which will be processed in the order they are received.
     * @param string to be sent to the server.
     */
    public static void push(String string) {
        // TODO validate message here.
        queue.add(string);
    }

    private void inner_loop() {
        Server_connection con = new Server_connection();
        while (true) { // Handles connecting and reconnecting, Should always run.
            con.openConnection();
            ExecutionLog executionLog = ExecutionLog.getInstance();
            while (con.checkConnection()) {

                try { // This makes the application work. It may not be touched.
                    Thread.sleep(1);
                } catch (InterruptedException e) {
                    System.err.println(e);
                }

                // Only do anything if there is a message to send.
                if (queue.isEmpty()) continue;

                // Sends the first message in queue.
                con.sendMessage(queue.poll());

                // Blocks until a message is received.
                String result = con.receiveMessage();

                Platform.runLater(() -> {
                    // Decodes the message and executions it's result.
                    Decode decode = new Decode(result);
                    decode.execute();
                });
            }

            // Clears the ExecutionLog and prints no connection, if you are disconnected.
            Platform.runLater(() -> {
                executionLog.clear();
                executionLog.fwd("No connection established");
            });

            try { // Sleep for for a longer duration before attempting to reconnect.
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                System.err.println(e);
            }
        }
    }

    // Test example to send a diagram to the server.
    public static void test() {
        Net.push("{1, [\"gateway:g\",\"user:u1\",\"user:u2\",\"user:u3\"], " +
                "[g, u1, u2, u3], [{u1, g,[fwd, u2, msg1]},{u3, g,[fwd, u1, msg2]},{g, u2,[fwd, u2, msg1]},{g, u1,[fwd, u1, msg2]}]}");
    }
}
