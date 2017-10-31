package net;

import javafx.application.Platform;
import visuals.DiagramView;
import visuals.Draw;
import visuals.ExecutionLog;

import static visuals.DiagramView.tabPane;

/**
 * @author Pontus Laestadius
 * @version 1.0
 */
class Decode {
    String raw; // Raw string to be decoded.

    /**
     * @param string string to decode.
     */
    Decode(String string) {
        raw = string;
    }

    /**
     * Decodes the raw string and allocates it to it's associated diagram.
     */
    void execute() {
        // If no string has been allocated, abort.
        if (raw == null) return;

        // Split the raw string in to fields.
        int id_index = raw.indexOf(",");

        String id_string = raw.substring(1, id_index);  // Retrieves the Diagram_ID.
        int id = Integer.decode(id_string); // String to Integer.

        if (raw.contains("simulation_finished")) { // If the simulation is finished.
            write("Simulation finished"); // Write Simulation finished in the execution log.

        } else if (raw.contains("print_information")) { // If it's a statement to only print.
            write("INFO: " + retrieveMessage(raw)); // Writes the raw data TODO replace with statement provided.

        } else { // If it's a message or a list of classes.
            String message = retrieveMessage(raw);
            String values = raw.replace(message, ""); // Removes the message content from raw.
            byte[] bytes = raw.getBytes(); // Gets the raw message as bytes.

            // Identifies if it's a message or a new diagram by the next to last byte being a integer or not.
            if (bytes[bytes.length-2] >= (byte)'0' && bytes[bytes.length-2] <= (byte)'9') { // Message

                // Adds message to the DiagramView.
                message(
                        DiagramView.getDiagramViewInView().getDraw(),// TODO change if you want multiple diagrams.
                        message, // The message content.
                        values.split(",")); // Split the fields remaining.
            } else { // New Diagram
                diagramClasses(message);
            }
        }
    }

    /**
     * Removes all the characters provided from the string provided.
     * @param string the string to purify.
     * @param characters The main card, "Exodia the Forbidden One",
     *                   is one of the first five Effect Monsters
     *                   ever to be released and the first card to
     *                   have an alternative victory condition.
     * @return a String without any of the characters provided.
     */
    private String removeCharactersFromString(String string, char... characters) {
        StringBuilder stringBuilder = new StringBuilder();
        boolean booleanToDetermineIfCharacterExistsOrNot = false;
        for (char input: string.toCharArray()) {
            for (char remove: characters) {
                if (input == remove) {
                    booleanToDetermineIfCharacterExistsOrNot = true;
                    break;
                }
            }
            if (!booleanToDetermineIfCharacterExistsOrNot)
                stringBuilder.append(input);
            booleanToDetermineIfCharacterExistsOrNot = false;
        }
        return stringBuilder.toString();
    }

    /**
     * Given a string following the network protocol, will create a new diagram with the provided classes.
     * @param classes to draw up the diagram with.
     */
    private void diagramClasses(String classes) {
        // TODO check if diagram_id is unique

        DiagramView diagramView = new DiagramView("diagram 1");
        tabPane.getTabs().add(diagramView.getTab());
        Draw draw = diagramView.getDraw();

        Platform.runLater(() -> {
            int numberOfClasses = 0;
            for (String s: classes.split(",")) {
                draw.addClass(removeCharactersFromString(s, '[', ']', '\"'));
                numberOfClasses+=1;
            }
            write("CREATED: Added " + numberOfClasses + " classes");

        });

    }

    /**
     * Adds a message to a Draw object.
     * @param draw draw to add the message too.
     * @param message content and what to display to send.
     * @param values includes the nodes it traverses.
     */
    private void message(Draw draw, String message, String[] values) {
        // Remove spaces from the from & to.
        String from_name = removeCharactersFromString(values[1], ' ');
        String to_name = removeCharactersFromString(values[2], ' ');
        // Gets the index of the class.
        int to = draw.findClassIndex(to_name);
        int from = draw.findClassIndex(from_name);

        // Adds the message and notifies the execution log.
        Platform.runLater(() -> {
            // If the message is invalid
            if (from == -1 || to == -1) {
                if (from == -1)
                    write("INVALID: {" + from_name + "} -> " + to_name + " | " + message);
                else
                    write("INVALID: " + from_name + " -> {" + to_name + "} | " + message);
            } else {
                write(from_name + " -> " + to_name + " | " + message);
                draw.addMessage(from, to, message);
            }
        });


    }

    /**
     * Writes the given content to the execution log.
     * @param string the string to display in the execution log.
     */
    private void write(String string) {
        Platform.runLater(() -> {
            // Gets the instance and writes a new line.
            ExecutionLog.getInstance().fwd(string);
        });
    }

    private String retrieveMessage(String string) {
        int msg_start = string.indexOf("["); // Finds the end of the message content.
        int msg_end = string.indexOf("]"); // Finds the start of the message content.
        return string.substring(msg_start+1, msg_end); // Message content. as a substring.
    }
}
