package visuals;

import javafx.scene.control.Tab;

import java.util.ArrayList;

/*
 * Handles a single tabbed diagram view and it's state
 * @version 0.1
 * @author Pontus Laestadius
 */
public class DiagramView {
    private Draw draw;
    private String tabName;
    private State state = State.PAUSED;
    public static ArrayList<DiagramView> list = new ArrayList<>(); // A list of all Diagram views.

    public DiagramView(Draw draw, String tabName) {
        this.draw = draw;
        this.tabName = tabName;
        list.add(this);
    }

    public Tab getTab() {
        Tab tab = new Tab();
        tab.setText(this.tabName);
        tab.setContent(draw.getCanvas());
        return tab;
    }

    public void resize(String observable, int value) {
        draw.resize((observable.contains("width") ? "width":"height"), value);
    }

}

enum State {
    WAITING, // Waiting for a response from the server.
    PAUSED, // Manually paused, no action is being performed.
    EXECUTING, // Executing the visual aspects.
}
