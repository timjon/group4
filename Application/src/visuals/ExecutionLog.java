package visuals;

import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.*;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.paint.Color;
import javafx.scene.control.ListView;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Displays any information given in a ListView.
 * @author Pontus Laestadius
 * @version 1.3
 */
public class ExecutionLog extends ListView {
    private static ExecutionLog elog; // Static singleton implementation.
    private ListView<String> listView = new ListView<>(); // Contains the content of the log.
    private ObservableList<String> data = FXCollections.observableArrayList(); // Contains the data of the fields.

    private boolean automatic = true;

    /**
     * @return the static instance of the Execution Log.
     */
    public static ExecutionLog getInstance() {
        return elog;
    }

    /**
     * This is a part of Execution Log. A class which displays a log on the left side of the screen. (setLeft). And it's
     * called in the Next button to identify if the simulation has been finished or not.
     * @return a boolean which is a true or false. True will be returned if the string matches the finish string.
     * False if it does not match.
     */
    public boolean isFinished() {
        return data.size() != 0 && data.get(data.size()-1).toLowerCase().equals("info: simulation finished");
    }

    /**
     * Only available constructor.
     * Creates a new execution log if one has not been initiated.
     * Sets it's visual properties.
     */
    public ExecutionLog() {
        if (elog != null) return; // Only allow a single ExecutionLog view.
        elog = this;
        this.setEditable(false);
        this.setWidth(250);
        Color c = Color.TRANSPARENT;
        BackgroundFill bgf = new BackgroundFill(c, null,null);
        Background bg = new Background(bgf);
        this.listView.setBackground(bg);

        // Adds listener for selecting a item.
        this.listView.
                // Gets the box which you can select in the overall list view.
                getSelectionModel().
                // Selects the property of selecting an item.
                selectedItemProperty().
                // Adds a listener for the property when it changes.
                addListener
                        // Scope and variables received when a change occurs in the listened property.
                ((ObservableValue<? extends String> observable, String oldValue, String newValue) -> {

                    // Get draw object.
                    Draw draw = DiagramView.getDiagramViewInView().getDraw();

                    // If you selected an INFO. do no action.
                    if (newValue.startsWith("INFO:")) {
                        return;
                    }

                    // Gets a list of the messages.
                    List<String> filteredList = filteredList();

                    // Keeps track of number of messages to go back.
                    int goBackNumberOfMessages = 0;

                    // Iterate over the filtered items.
                    for (String string: filteredList) {

                        // Match for equality.
                        if (string.equals(newValue))
                            break;

                        // Iterates messages to go back.
                        goBackNumberOfMessages++;

                    }

                    // Remove the number of filtered items.
                    goBackNumberOfMessages = filteredList.size()-goBackNumberOfMessages;

                    // If you did not go back any steps, return to normal execution.
                    if (goBackNumberOfMessages < filteredList.size() - (automatic?0:-1)) {

                        // Convert steps to index.
                        goBackNumberOfMessages -= 1;

                        // Get the message we are going to display.
                        Message message = draw.getMessage(goBackNumberOfMessages);

                        // Set it to be static.
                        message.setStatic(true);

                    }

                    // Manual change.
                    automatic = false;

                });

    }

    /**
     * Forwards
     * Adds an item to the ListView.
     */
    public void fwd(String line) {
        data.add(line);
        update();
    }

    /**
     * @return a filtered list without any information/filler texts.
     */
    private List<String> filteredList() {
        // Predicate the data list.
        List<String> filteredList =
                // Gets the stream of Strings.
                data.stream().
                        // Filter away INFO messages.
                                filter(s -> !s.startsWith("INFO:"))
                        // Collect it to the list.
                        .collect(Collectors.toList());

        // Reverse the order of the items.
        Collections.reverse(filteredList);

        return filteredList;
    }

    /**
     * Backwards
     * Removes the last item from the ListView.
     */
    public void bwd() {

        // If there are no messages to be removed.
        if (filteredList().size() == 0) return;

        for (int i = data.size()-1; i >= 0; i--) {
            String element = data.remove(data.size()-1);
            if (!element.startsWith("INFO:"))
                break;
        }

        // Removes all INFO before it, ignoring spawning messages.
        for (int i = data.size()-1; i >= 0; i--) {
            if (!data.get(i).startsWith("INFO:") || data.get(i).toLowerCase().contains("spawned"))
                break;
            data.remove(i);
        }

        update();
    }

    /**
     * Removes all items from the ListView.
     */
    public void clear() {
        data = FXCollections.observableArrayList();
        update();
    }

    /**
     * Updates the data in the ListView.
     */
    private void update() {
        listView.setItems(data); // Updates the content of the list.
        listView.scrollTo(data.size()-1); // Scrolls to the bottom of the list.
        SelectionModel<String> model = listView.getSelectionModel();
        model.selectLast(); // Selects the last item in the list.
        automatic = true; // Automatically on.
    }

    /**
     * @return a container ListView.
     */
    public ListView<String> getContainer() {
        return listView;
    }

    /**
     * @param data sets content of the ExecutionLog.
     */
    void setData(ObservableList<String> data) {
        this.data = data;
        update();
    }
}