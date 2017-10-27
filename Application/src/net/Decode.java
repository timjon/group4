package net;

import javafx.application.Platform;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;

import static visuals.DiagramView.tabPane;

/**
 * @author Pontus Laestadius
 */
class Decode {
    String raw;
    static Draw draw;

    Decode(String string) {
        raw = string;
    }

    void execute() {
        if (raw == null) return;

        if (raw.contains("simulation_finished")) {
            // Clears the ExecutionLog and prints no connection, if you are disconnected.
            write(raw);
        } else if (raw.contains("print_information")) { // If it's a statement to only print.
            write(raw);
            // TODO needs proper ID.
            Net.push("{1, send_message}");

        } else { // If it's a message or a list of classes.

            System.out.println("raw:" + raw);
            int msg_start = raw.indexOf("[");
            int msg_end = raw.indexOf("]");
            String msg = raw.substring(msg_start, msg_end);

            String values = raw.replace(msg, "");
            String[] split_values = values.split(",");

            byte[] bytes = raw.getBytes();

            if (bytes[bytes.length-2] >= (byte)'0' && bytes[bytes.length-2] <= (byte)'9') { // Message
                Platform.runLater(() -> {
                    Draw d = DiagramView.getDiagramViewInView().getDraw();
                    int from = d.messageNameToMessageInt(removeCharactersFromString(split_values[1], ' '));
                    int to = d.messageNameToMessageInt(removeCharactersFromString(split_values[2], ' '));
                    d.addMessage(from, to, msg);// TODO string to numberings.
                });

            } else { // New Diagram
                // TODO check if diagram_id is unique

                DiagramView dv = new DiagramView("diagram 1");
                tabPane.getTabs().add(dv.getTab());

                Platform.runLater(() -> {
                    draw = dv.getDraw();
                    for (String s: msg.split(",")) {
                        draw.addClass(removeCharactersFromString(s, '[', ']', '\"'));
                    }
                    tabPane.getTabs().add(dv.getTab());
                });

            }
        }

    }

    /**
     * Removes all the characters provided from the string provided.
     * @param string the string to purify.
     * @param characters characters to remove
     * @return a String without any of the characters provided.
     */
    private String removeCharactersFromString(String string, char... characters) {
        StringBuilder sb = new StringBuilder();
        for (char ch: string.toCharArray()) {
            for (char ch2: characters)
                if (ch == ch2) break;
            sb.append(ch);
        }
        return sb.toString();
    }

    /**
     * Writes the given content to the execution log.
     * @param string the string to display in the execution log.
     */
    private void write(String string) {
        Platform.runLater(() -> {
            ExecutionLog.getInstance().fwd(string);
        });
    }
}
