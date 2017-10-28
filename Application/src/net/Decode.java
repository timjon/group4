package net;

import javafx.application.Platform;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;

import static visuals.DiagramView.tabPane;

/**
 * @author Pontus Laestadius
 * @version 0.1
 */
class Decode {
    String raw;
    static Draw draw;

    Decode(String string) {
        raw = string;
    }

    void execute() {
        if (raw == null) return;
        int id_index = raw.indexOf(",");
        String id_string = raw.substring(1, id_index);
        int id = Integer.decode(id_string);

        if (raw.contains("simulation_finished")) { // If the simulation is finished.
            write("Simulation finished");

        } else if (raw.contains("print_information")) { // If it's a statement to only print.
            write(raw);
            // TODO needs proper ID.
            Net.push("{1, send_message}");

        } else { // If it's a message or a list of classes.
            int msg_start = raw.indexOf("[");
            int msg_end = raw.indexOf("]");
            String msg = raw.substring(msg_start, msg_end);

            String values = raw.replace(msg, "");
            byte[] bytes = raw.getBytes();

            if (bytes[bytes.length-2] >= (byte)'0' && bytes[bytes.length-2] <= (byte)'9') { // Message
                message(msg, values.split(","));
            } else { // New Diagram
                diagramClasses(msg);
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
        boolean e = false;
        for (char ch: string.toCharArray()) {
            for (char ch2: characters) {
                if (ch == ch2) {
                    e = true;
                    break;
                }
            }
            if (!e)
                sb.append(ch);
            e = false;
        }
        return sb.toString();
    }

    /**
     * Given a string following the network protocol, will create a new diagram with the provided classes.
     * @param classes to draw up the diagram with.
     */
    private void diagramClasses(String classes) {
        // TODO check if diagram_id is unique

        DiagramView dv = new DiagramView("diagram 1");
        tabPane.getTabs().add(dv.getTab());
        draw = dv.getDraw();

        Platform.runLater(() -> {
            for (String s: classes.split(",")) {
                draw.addClass(removeCharactersFromString(s, '[', ']', '\"'));
            }
        });
    }

    private void message(String message, String[] values) {
        Draw d = DiagramView.getDiagramViewInView().getDraw();
        int from = d.messageNameToMessageInt(removeCharactersFromString(values[1], ' '));
        int to = d.messageNameToMessageInt(removeCharactersFromString(values[2], ' '));
        Platform.runLater(() -> {
            write(from + " -> " + to + "msg: " + message);
            d.addMessage(from, to, message);
        });
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
