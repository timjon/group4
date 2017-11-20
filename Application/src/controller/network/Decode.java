package controller.network;

import javafx.application.Platform;
import model.Menu;
import view.DiagramView;
import view.visuals.Draw;
import view.ExecutionLog;

import static view.DiagramView.tabPane;

/**
 * @author Pontus Laestadius
 * @version 1.3
 */
class Decode {
    // Raw string to be decoded.
    private String rawStringToDecode;

    /**
     * @param string string to decode.
     */
    Decode(String string) {
        rawStringToDecode = string;
    }

    /**
     * Decodes the rawStringToDecode string and allocates it to it's associated diagram.
     */
    void execute() {
        // If no string has been allocated, abort.
        if (rawStringToDecode == null) return;

        // Split the rawStringToDecode string in to fields.
        int id_index = rawStringToDecode.indexOf(",");

        // Retrieves the Diagram_ID.
        String id = rawStringToDecode.substring(1, id_index);

        // If the simulation is finished.
        if (rawStringToDecode.contains("simulation_finished")) {

            // Write Simulation finished in the execution log.
            write(id, "INFO: Simulation finished");

            // Update menu state.
            Menu.getInstance().identifyState();

            // If it's a previous message confirmation.
        } else if (rawStringToDecode.contains("previous_confirmation")) {

            // Only go back if you can remove a message.
            if (DiagramView.getDiagramViewInView().getDraw().removeMessage()) {

                // Remove a line from the execution log.
                Platform.runLater(() -> {
                    ExecutionLog.getInstance().bwd();
                });
            } else {
                System.out.println("This should not occur.");
            }

            Menu.getInstance().identifyState();

        } else if (rawStringToDecode.contains("print_information")) {

            // Writes the rawStringToDecode data
            write(id, "INFO: " + retrieveMessage(rawStringToDecode));

            // If it's a message or a list of classes (new diagram).
        } else {

            // Gets the message content.
            String message = retrieveMessage(rawStringToDecode);

            // Removes the message content from rawStringToDecode.
            String values = rawStringToDecode.replace(message, "");

            // Gets the rawStringToDecode message as bytes.
            byte[] bytes = rawStringToDecode.getBytes();

            // Identifies if it's a message or a new diagram by the next to last byte being a integer or not.

            // Message
            if (bytes[bytes.length-2] >= (byte)'0' && bytes[bytes.length-2] <= (byte)'9') {

                // Adds message to the DiagramView.
                message(
                        id,
                        DiagramView.getDiagramViewInView().getDraw(),
                        message, // The message content.
                        values.split(",")); // Split the fields remaining.

                // New Diagram
            } else {
                diagramClasses(message, id);
            }
        }
    }

    /**
     * Removes all the characters provided from the string provided.
     * @param string the string to purify.
     * @param characters to be excluded from the result string.
     * @return a String without any of the characters provided.
     */
    private String removeCharactersFromString(String string, char... characters) {

        // Use a StringBuilder for improved string concat.
        StringBuilder stringBuilder = new StringBuilder();

        // Holds the state, if the character existed in the string provided.
        boolean booleanToDetermineIfCharacterExistsOrNot = false;

        // Iterate over the characters in the input string.
        for (char input: string.toCharArray()) {

            // Iterate over the characters to remove.
            for (char remove: characters) {

                // If the character is going to removed.
                if (input == remove) {

                    // Set the state flag to true.
                    booleanToDetermineIfCharacterExistsOrNot = true;
                    break;
                }

            }

            // If the character is not in the character list.
            if (!booleanToDetermineIfCharacterExistsOrNot)

                // Append it to the string builder.
                stringBuilder.append(input);

            // Reset value in between iterations.
            booleanToDetermineIfCharacterExistsOrNot = false;

        }

        // re assemble the remaining characters in the same order.
        return stringBuilder.toString();
    }

    /**
     * Given a string following the network protocol, will create a new diagram with the provided classes.
     * @param classes to draw up the diagram with.
     */
    private void diagramClasses(String classes, String id) {
        // TODO check if diagram_id is unique

        // Creates a new view with the tab name.
        DiagramView diagramView = new DiagramView("SDid: " + id, id);

        // Add the tab to the collection of tabs.
        tabPane.getTabs().add(diagramView.getTab());

        // Retrieve the draw object to add the classes too.
        Draw draw = diagramView.getDraw();


        Platform.runLater(() -> {

            // Count number of classes added.
            int numberOfClasses = 0;
            
            // Store the separate classes.
            String[] splitClasses = classes.split(",");
            
            // Get the first class only.
            String actorClass_name = removeCharactersFromString(splitClasses[0], '[', ']', '\"');
            
            // Add the first class to the draw object.
            draw.addActor(actorClass_name);
            
            // Increment number of classes drawn.
            numberOfClasses+=1;

            // Split the remaining fields and add them as classes.
            for (int i = 1; i < splitClasses.length; i++) {

                // Removes a few specific characters that may appear in the class name.
                String diagramClass_name = removeCharactersFromString(splitClasses[i], '[', ']', '\"');

                // Add the class to the draw object.
                draw.addClass(diagramClass_name);

                // Iterate number of classes drawn.
                numberOfClasses+=1;
            }

            // Display in the execution log the number of classes created.
            write(id, "INFO: created " + numberOfClasses + " classes");

            // Turns on animations.
            Draw.animate(true);

        });

    }

    /**
     * Adds a message to a Draw object.
     * @param draw draw to add the message too.
     * @param message content and what to display to send.
     * @param values includes the nodes it traverses.
     */
    private void message(String id, Draw draw, String message, String[] values) {

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
                    write(id, "INVALID: {" + from_name + "} -> " + to_name + " | " + message);

                else // to == -1
                    write(id, "INVALID: " + from_name + " -> {" + to_name + "} | " + message);

                // If it's a valid message.
            } else {

                // Write to the execution log.
                write(id, from_name + " -> " + to_name + " | " + message);

                // Add it to the diagram.
                draw.addMessage(from, to, message);
            }

        });

    }

    /**
     * Writes the given content to the execution log.
     * @param string the string to display in the execution log.
     */
    private void write(String id, String string) {

        Platform.runLater(() -> {

            // Gets the instance and writes a new line.
            // Gets the correct diagramView and adds the data to it's log.
            for (DiagramView diagramView: DiagramView.diagramViews) {
                if (diagramView.getTab().getId().equals(id)){

                    // Adds to the current visable log.
                    if (diagramView.getTab() == DiagramView.getDiagramViewInView().getTab()) {
                        ExecutionLog.getInstance().fwd(string);

                        // Just stores the data.
                    } else {
                        diagramView.addLogData(string);
                    }
                }
            }

            Menu.getInstance().identifyState();
        });

    }

    /**
     * retrieves a message incapsulated in a list. i.e [message].
     * @param string to find the message inside.
     * @return the content of the message.
     */
    private String retrieveMessage(String string) {

        // Finds the start of the message content.
        int msg_start = string.indexOf("[");

        // Finds the end of the message content.
        int msg_end = string.indexOf("]");

        // Message content as a substring.g
        return string.substring(msg_start+1, msg_end);
    }
}
