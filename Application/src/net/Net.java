package net;

import visuals.ExecutionLog;
import javafx.application.Platform;
import visuals.handlers.Animation;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Pontus Laestadius
 * @version 0.6
 */
public class Net implements Runnable {
    private static Server_connection con = null;
    private static List<String> queue = new ArrayList<>();

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

    public static void push(String string) {
        // TODO validate message here.
        queue.add(string);
    }

    private void inner_loop() {
        Server_connection con = new Server_connection();
        while (true) {
            con.openConnection();
            ExecutionLog executionLog = ExecutionLog.getInstance();
            while (con.checkConnection()) {
                // Only do anything if there is a message to send.
                if (queue.isEmpty()) continue;

                con.sendMessage(queue.remove(0));

                // Blocks until a message is received.
                String result = con.receiveMessage();

                // Updates the ExecutionLog.
                Platform.runLater(() -> {
                    executionLog.fwd(result);
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

    public static void test() {
        Net.push("{1, [\"gateway:g\",\"user:u1\",\"user:u2 \",\"user:u3\"], [g, u1, u2, u3], [{u1, g,[fwd, u2, msg1]},{u3, g,[fwd, u1, msg2]},{ g, u2,[fwd, u2, msg1]},{g, u1,[fwd, u1, msg2]}]}");

        for (int i = 0; i < 5; i++) {
            Net.push("{1, send_message}");
        }

    }
}
