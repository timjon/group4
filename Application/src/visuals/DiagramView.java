package visuals;

import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;

import java.util.ArrayList;

/*
 * Handles a single tabbed diagram view and it's state
 * @version 0.5
 * @author Pontus Laestadius
 */
public class DiagramView {
    private Draw draw;
    private String tabName;
    private State state = State.PAUSED;
    private Tab tab;
    public static ArrayList<DiagramView> list = new ArrayList<>(); // A list of all Diagram views.
    public static TabPane tabPane;

    public Draw getDraw() {
        return draw;
    }

    /**
     * @param name to match.
     * @return true if the provided name is the selected tab.
     */
    public static boolean inView(String name) {
        return getInView().equals(name);
    }

    public static String getInView() {
        return tabPane.getSelectionModel().getSelectedItem().getText();
    }

    // TODO this might animate the wrong tab if they have the same name.
    public static Draw getDrawInView() {
        for (DiagramView d: list) {
            if (d.getDraw().getName().equals(getInView())) {
                return d.getDraw();
            }
        }
        return null;
    }

    /**
     * Creates a DiagramView with an Initial new Draw object with the size of the static TabPane.
     * Adds it to the list of DiagramViews and
     * @param tabName the diagram name.
     */
    public DiagramView(String tabName) {
        this.draw = new Draw(tabName, (int)tabPane.getWidth(), (int)tabPane.getHeight());
        this.tabName = tabName;
        list.add(this);
        this.tab = new Tab();
        tab.setText(this.tabName);
        tab.setContent(draw.getCanvas());
    }

    /**
     * @return the tab containing the Draw object handled by the DiagramView.
     */
    public Tab getTab() {
        return this.tab;
    }

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

    State setState(State state) {
        State tmp = this.state;
        this.state = state;
        return tmp;
    }

}

enum State {
    WAITING, // Waiting for a response from the server.
    PAUSED, // Manually paused, no action is being performed.
    EXECUTING, // Executing the visual aspects.
    RESIZING, // While resizing to avoid concurrency issues.

}
