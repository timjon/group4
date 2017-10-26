package visuals;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.*;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.paint.Color;
import javafx.scene.control.ListView;

/**
 * @author Pontus Laestadius
 * @version 0.4
 */
public class ExecutionLog extends ListView {
    public static ExecutionLog elog;
    private ListView<String> listView = new ListView<>();
    private ObservableList<String> data = FXCollections.observableArrayList();

    public ExecutionLog() {
        elog = this;
        this.setEditable(false);
        this.setWidth(250);

        Color c = Color.BLACK;
        BackgroundFill bgf = new BackgroundFill(c, null,null);
        Background bg = new Background(bgf);
        this.listView.setBackground(bg);
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
     * Backwards
     * Removes the last item from the ListView.
     */
    public void bwd() {
        data.remove(data.size()-1);
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
        listView.setItems(data);
        listView.scrollTo(data.size()-1);
        SelectionModel<String> model = listView.getSelectionModel();
        model.selectLast();
    }

    /**
     * @return a container ListView.
     */
    public ListView<String> getContainer() {
        return listView;
    }
}

