package visuals;

import javafx.scene.control.Tab;

/*
 * Handles a single tabbed diagram view and it's state
 * @version 0.1
 * @author Pontus Laestadius
 */
public class DiagramView {
    private Draw draw;
    private String tabName;
    private State state = State.PAUSED;

    public DiagramView(Draw draw, String tabName) {
        this.draw = draw;
        this.tabName = tabName;
    }

    public Tab getTab() {
        Tab tab = new Tab();
        tab.setText(this.tabName);
        tab.setContent(draw.getCanvas());
        return tab;
    }
}

enum State {
    WAITING, // Waiting for a response from the server.
    PAUSED, // Manually paused, no action is being performed.
    EXECUTING, // Executing the visual aspects.
}
