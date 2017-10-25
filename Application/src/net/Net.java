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
            while (con.checkConnection()) {
                String result = con.receiveMessage();

                while (ExecutionLog.elog == null);
                Platform.runLater(() -> {
                    ExecutionLog.elog.fwd(result);
                });

                try {
                    Thread.sleep(700); // Sleep for the same time as timer increases
                } catch (InterruptedException e) {
                    System.err.println(e);
                }
                con.sendMessage("{1, next_message}");
            }
            try {
                Thread.sleep(700); // Sleep for the same time as timer increases
            } catch (InterruptedException e) {
                System.err.println(e);
            }
        }
    }
}
