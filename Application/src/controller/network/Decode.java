package controller.network;

import javafx.application.Platform;
import model.Menu;
import view.DiagramView;
import view.visuals.Draw;
import view.ExecutionLog;

import static controller.Import.disp;
import static view.DiagramView.tabPane;

/**
 * @author Pontus Laestadius
 * Collaborators: Sebastian Fransson, Tim Jonasson, Kosara Golemshinska
 * @version 1.6
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
     * Decodes the rawStringToDecode string and allocates it to its associated diagram.
     */
    void execute() {
        // If no string has been allocated, abort.
        if (rawStringToDecode == null) return;

        // Deployment diagram has been found.
        if (rawStringToDecode.contains("deployment_diagram")) {

            // Remove the curly brackets.
            rawStringToDecode = removeCharactersFromString(rawStringToDecode, '{', '}', '"');

            // Removes whitespaces and new lines.
            rawStringToDecode = rawStringToDecode.replaceAll("\n", "").replaceAll("\\s", "");

            // Splits the input.
            String[] split = rawStringToDecode.split(",");

            // Start on the third value (ignoring the first, second and third.)
            for (int i = 3; i < split.length; i++) {

                // Remove not required characters.
                String field = removeCharactersFromString(split[i], '[', ']');
                String second = "";

                if (!field.contains("]")) {

                    // Iterate i as we pull the next item.
                    i += 1;

                    // Remove not required characters.
                    second = split[i].replace("]", "");
                }

                // Get the draw object.
                Draw draw = DiagramView.getDiagramViewInView().getDraw();

                // Add a diagram class to it.
                draw.addDeploymentDiagramClass(second);

                // Add a process device to it.
                draw.addProcessToDevice(second, field);
            }

            // Ignore the rest of decoding.
            return;

        }

        // Class diagram has been found.
        if (rawStringToDecode.contains("class_diagram")) {

            if (rawStringToDecode.contains("highlight")) {

                // Remove the curly brackets.
                rawStringToDecode = removeCharactersFromString(rawStringToDecode, '{', '}', '"');
                // Split into fields.
                String[] highlight_class_data = rawStringToDecode.split(",");
                // The name of the class is the last item in this array.
                String highlight = highlight_class_data[4];
                // New highlight.
                highlight(highlight);
                return;

            } else {
                // Replace all newlines and whitespaces in the string.

                rawStringToDecode = rawStringToDecode.replaceAll("\n", "")
                        .replaceAll("\\s", "");

                // Split the server output into 3 array items using the commas.
                String[] class_values = rawStringToDecode.split(",", 3);

                // Second array item is the class diagram ID.
                String related_s_ID = class_values[1];

                // Whole third item is the sequence diagram ID concatenated with the classes.
                String id_and_classes = class_values[2];

                // Get the index of the first comma.
                int c_ID_index = id_and_classes.indexOf(",");

                // Get the sequence diagram ID.
                String class_ID = id_and_classes.substring(0, c_ID_index);

                // Remove the CID.
                String attributes = id_and_classes.replace(class_ID, "");

                // Get the list without the comma in front of it.
                String attributes_list = attributes.substring(1);

                // Split the list into classes and relationships.
                String[] classes_and_relations = attributes_list.split("]],");

                // New class diagram sending all the classes in a String array.
                String[] class_diagram_classes = classes_and_relations[0].split("],");
                classDiagramClasses(class_diagram_classes, class_ID, related_s_ID);

                // New relationship list, supports inheritance only for now.
                if (classes_and_relations[1].contains("inheritance")) {

                    // Split relationship list into items.
                    String[] split_relations = classes_and_relations[1].split("],");

                    // Remove all brackets and quotes, then send as comma separated strings.
                    for (int i = 0; i < split_relations.length; i++) {
                        String class_relation = removeCharactersFromString(split_relations[i], '[', ']', '}', '\'');
                        // New relation.
                        classRelation(class_ID, class_relation);
                    }
                }
            }
        }


        // Split the rawStringToDecode string in to fields.
        int id_index = rawStringToDecode.indexOf(",");

        // Retrieves the ID of the diagram.
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

            //Info messages
        } else if (rawStringToDecode.contains("INFO#")) {
            //Send through the rawStringToDecode starting at the 6th character to skip the INFO#.
            disp("Info", rawStringToDecode.substring(6), "");
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
        //Updates the button states. Placed here to update the state even though there exists no diagramview yet.
        Menu.getInstance().identifyState();
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
     * Adds a class diagram to the Draw object.
     * @param classes used to draw up the diagram
     * @param id of the diagram
     * @param SDID the ID of the connected sequence diagram
     */
    private void classDiagramClasses(String[] classes, String id, String SDID) {

        // Retrieve the draw object to add the classes too.
        Draw draw = DiagramView.getDiagramViewInView().getDraw();

        Platform.runLater(() -> {

            // Remove brackets and quotes.
            for (int i = 0; i < classes.length; i++) {
                String single_class = removeCharactersFromString(classes[i], '[', ']', '\'');
                // Check if there are attributes
                if(classes[i].contains(",")) {
                    int name_index = single_class.indexOf(",");
                    // Split the name of the class.
                    String class_name = single_class.substring(0, name_index);
                    // Attributes list.
                    String fields = single_class.substring(name_index + 1);
                    // TODO implement the lines below in Draw
                    // String[] classFields = fields.split(",");
                    // draw.addClassFields(class_name, classFields);
                    // Add the class to the draw object.
                    draw.addClassDiagramClass(class_name);
                } else {
                    // Single class, no fields.
                    draw.addClassDiagramClass(single_class);
                }
            }

            // Turns on animations.
            Draw.animate(true);
        });
    }

    /**
     * Adds a relationship to the Draw object.
     * @param id of the diagram
     * @param relationship type between class diagra classes
     */
    private void classRelation(String id, String relationship) {

        // Retrieve the draw object to add the classes too.
        Draw draw = DiagramView.getDiagramViewInView().getDraw();

        Platform.runLater(() -> {

            // Split on comma.
            String[] single_relationship = relationship.split(",");

            // TODO implement after inheritance has been finished
            // draw.addClassDiagramRelation(single_relationship[1], single_relationship[2]);
        });
    }

    /**
     * Adds a highlight to a Draw object.
     * @param className name of the class to highlight
     */
    private void highlight(String className) {

        // Retrieve the draw object to add the classes too.
        Draw draw = DiagramView.getDiagramViewInView().getDraw();

        Platform.runLater(() -> {
            // Add the highlight to a Draw object.
            draw.highlightClass(className);
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
     * retrieves a message encapsulated in a list. i.e [message].
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
