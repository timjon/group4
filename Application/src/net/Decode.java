package net;

import javafx.application.Platform;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;

import static visuals.DiagramView.tabPane;

class Decode {
    String raw;

    Decode(String string) {
        raw = string;
    }

    void execute() {
        if (raw == null) return;

        if (raw.contains("simulation_finished")) {
            // Clears the ExecutionLog and prints no connection, if you are disconnected.
            Platform.runLater(() -> {
                ExecutionLog.getInstance().fwd(raw);
            });
        } else { // If it's a message or a list of classes.

            //System.out.println("raw:" + raw);
            int msg_start = raw.indexOf("[");
            int msg_end = raw.indexOf("]");
            String msg = raw.substring(msg_start, msg_end);

            String values = raw.replace(msg, "");
            String[] split_values = values.split(",");

            byte[] bytes = raw.getBytes();

            if (bytes[bytes.length-2] >= (byte)'0' && bytes[bytes.length-2] <= (byte)'9') { // Message

            } else { // New Diagram

                // TODO check if diagram_id is unique

                DiagramView dv = new DiagramView("diagram 1");
                tabPane.getTabs().add(dv.getTab());

                Platform.runLater(() -> {
                    Draw draw = dv.getDraw();
                    //System.out.println(msg);
                    for (String s: msg.split(",")) {
                        //System.out.println(s);
                        draw.addClass(s);
                    }
                    tabPane.getTabs().add(dv.getTab());
                });

            }
        }

    }
}
