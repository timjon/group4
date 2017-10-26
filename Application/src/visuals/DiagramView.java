package visuals;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import visuals.handlers.UniqueCounter;

import java.util.ArrayList;

/*
 * Handles a single tabbed diagram view and it's state
 * @version 0.7
 * @author Pontus Laestadius
 */
public class DiagramView {
    public static TabPane tabPane;

    private Draw draw;
    private String tabName;
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
        DiagramView dv = getDiagramViewInView();
        return dv != null && dv.getDraw() == draw;
    }

    /**
     * @return the id of the current selected tab.
     */
    static String getInView() {
        return tabPane.getSelectionModel().getSelectedItem().getId();
    }

    /**
     * @return the DiagramView that is currently being viewed. null if none.
     */
    public static DiagramView getDiagramViewInView() {
        for (DiagramView d: diagramViews) {
            Tab t = d.getTab();
            String id = t.getId();
            if (id.equals(getInView())) {
                return d;
            }
        }
        return null;
    }

    /**
     * Creates a DiagramView with an Initial new Draw object with the size of the static TabPane.
     * Adds it to the diagramViews of DiagramViews and
     * @param tabName the diagram name.
     */
    public DiagramView(String tabName) {
        this.draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        this.tabName = tabName;
        diagramViews.add(this);
        this.tab = new Tab();
        tab.setId(UniqueCounter.getString());
        tab.setText(this.tabName);
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

    public void focus() {
        ExecutionLog.elog.setData(logData);
    }

    public void setlogData(ObservableList<String> logData) {
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
