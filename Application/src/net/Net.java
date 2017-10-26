package net;

import visuals.ExecutionLog;
import javafx.application.Platform;

/**
 * @author Pontus Laestadius
 * @version 0.3
 */
public class Net implements Runnable {
    private static Server_connection con = null;

    @Override
    public void run() {
        inner_loop();
    }

    public static void init() {
        if (con == null && Platform.isFxApplicationThread())
            (new Thread(new Net())).start();
    }

    private void inner_loop() {
        Server_connection con = new Server_connection();
        String temp = "{1, [\"gateway:g\",\"user:u1\",\"user:u2\",\"user:u3\"], [g, u1, u2, u3], [{u1, g,[fwd, u2, msg1]},{u3, g,[fwd, u1, msg2]},{ g, u2,[fwd, u2, msg1]},{g, u1,[fwd, u1, msg2]}]}";
        while (true) {
            con.openConnection();
            con.sendMessage(temp);
            ExecutionLog executionLog = ExecutionLog.getInstance();
            while (con.checkConnection()) {
                // Blocks until a message is recieved.
                String result = con.receiveMessage();

                Platform.runLater(() -> {
                    executionLog.fwd(result);
                });


                try {
                    Thread.sleep(1000); // Sleep for the same time as timer increases
                } catch (InterruptedException e) {
                    System.err.println(e);
                }

                con.sendMessage("{1, next_message}");
            }

            Platform.runLater(() -> {
                executionLog.clear();
                executionLog.fwd("No connection established");
            });

            // Sleep for for a longer duration before attempting to reconnect.
            try {
                Thread.sleep(3000); // Sleep for the same time as timer increases
            } catch (InterruptedException e) {
                System.err.println(e);
            }
        }
    }
}
