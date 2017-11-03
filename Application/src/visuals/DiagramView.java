package visuals;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;

import java.util.ArrayList;

/**
 * Handles a single tabbed diagram view and it's state
 * @author Pontus Laestadius
 * @version 1.0
 */
public class DiagramView {
    public static TabPane tabPane;

    private Draw draw;
    private State state = State.PAUSED;
    private Tab tab;

    // Stores the execution log data for this diagram.
    private ObservableList<String> logData = FXCollections.observableArrayList();
    public static ArrayList<DiagramView> diagramViews = new ArrayList<>(); // A diagramViews of all Diagram views.

    /**
     * @return a Draw Object.
     */
    public Draw getDraw() {
        return draw;
    }

    /**
     * Returns if the input Draw Object is currently being viewed.
     * @param draw a Draw Object to match with.
     * @return true if it is in view. false if a DiagramView does not exist for the Draw or if it is not in view.
     */
    static boolean inView(Draw draw) {
        DiagramView diagramView = getDiagramViewInView();
        return diagramView.getDraw() == draw;
    }

    /**
     * @return the id of the current selected tab.
     */
    private static String getInView() throws NullPointerException {
        return tabPane.getSelectionModel().getSelectedItem().getId();
    }

    /**
     * @return the DiagramView that is currently being viewed. null if none.
     */
    public static DiagramView getDiagramViewInView() throws IllegalStateException, NullPointerException {
        // If there are no diagrams added to the view list.
        if (diagramViews.size() == 0)
            throw new IllegalStateException("No views exist");

        // Iterate over all the available lists.
        for (DiagramView d: diagramViews)
            // Compare the tab id with the tab id of selected (in view) tab.
            if (d.getTab().getId().equals(getInView()))
                return d;

        // If the view is not in the view list.
        throw new IllegalStateException("View not in view list");
    }

    /**
     * Creates a DiagramView with an Initial new Draw object with the size of the static TabPane.
     * Adds it to the diagramViews of DiagramViews and
     * @param tabName the diagram name.
     */
    public DiagramView(String tabName, String id) {
        this.draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        diagramViews.add(this);
        this.tab = new Tab();
        // Get a unique number for the ID.
        tab.setId(id);
        tab.setText(tabName);
        tab.setContent(draw.getCanvas());
    }

    /**
     * @return the tab containing the Draw object handled by the DiagramView.
     */
    public Tab getTab() {
        return this.tab;
    }

    /**
     * Calls the internal redraw method of the Draw Object.
     */
    public void redraw() {
        draw.redraw();
    }

    /**
     * Resizes the Draw object to with tabpane's dimensions.
     */
    public void resize() {
        // By adding the Resizing state and checking for it, All concurrency related bugs were squashed.
        if (this.state == State.RESIZING) return;
        State tmp = setState(State.RESIZING);
        draw.resize(tabPane.getWidth(),tabPane.getHeight());
        this.state = tmp;
    }

    /**
     * Sets the state to the provided one and returns the previously allocated state.
     * @param state the new state
     * @return the previous state
     */
    private State setState(State state) {
        State previousState = this.state;
        this.state = state;
        return previousState;
    }

    /**
     * Allocates and displays the ExecutionLog for the current DiagramView.
     */
    public void focus() {
        Platform.runLater(() -> {
            // Sets the execution log to display the data of this diagram view log.
            ExecutionLog.getInstance().setData(logData);
        });
    }

    /**
     * @param logData data for the execution log.
     */
    public void setLogData(ObservableList<String> logData) {
        this.logData = logData;
    }
}

/**
 * Lists all the States of the DiagramView.
 */
enum State {
    WAITING, // Waiting for a response from the server.
    PAUSED, // Manually paused, no action is being performed.
    EXECUTING, // Executing the visual aspects.
    RESIZING, // While resizing to avoid concurrency issues.
}
