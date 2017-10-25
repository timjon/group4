package visuals;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.*;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.paint.Color;
import javafx.scene.control.ListView;
import javafx.scene.text.Font;

/**
 * @author Pontus Laestadius
 * @version 0.3
 */
public class ExecutionLog extends ListView {
    public static ExecutionLog elog;

    ListView<String> listView = new ListView<>();
    ObservableList<String> data = FXCollections.observableArrayList
            ("----- Start of Execution -----");

    static Font font = new Font("courier", 12);

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
     * Adds an item to the ExecutionLog.
     * Forwards
     */
    public void fwd(String line) {
        data.add(line);
        update();
    }

    /**
     * Backwards
     */
    public void bwd() {
        data.remove(data.size()-1);
        update();
    }

    private void update() {
        listView.setItems(data);
        listView.scrollTo(data.size()-1);
        SelectionModel<String> model = listView.getSelectionModel();
        model.selectLast();
    }

    public ListView<String> getContainer() {
        return listView;
    }
}
