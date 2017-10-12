package visuals;

import javafx.scene.control.Tab;

/*
 * Handles a single tabbed diagram view and it's state
 * @version 0.1
 * @author Pontus Laestadius
 */
public class DiagramView {
    Draw draw;
    String tabName;
    // TODO Add state.

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
