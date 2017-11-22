package view;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.BorderPane;
import view.visuals.Draw;

import java.util.ArrayList;

/**
 * Handles a single tabbed diagram view and it's state
 * @author Pontus Laestadius
 * @version 2.0
 */
public class DiagramView {
    public static TabPane tabPane;

    private Draw draw;
    private Tab tab;

    // Used as enums for identifying what diagram to add.
    private final String SEQUENCE_DIAGRAM = "SEQUENCE_DIAGRAM";
    public final String CLASS_DIAGRAM = "CLASS_DIAGRAM";
    public final String DEPLOYMENT_DIAGRAM = "DEPLOYMENT_DIAGRAM";

    // List of diagrams that are currently being viewed.
    private ArrayList<String> viewing = new ArrayList<>();

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
    public static boolean inView(Draw draw) {
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

        // Adds it to the list of retrievable DiagramViews.
        diagramViews.add(this);

        this.tab = new Tab();

        // Get a unique number for the ID.
        tab.setId(id);
        tab.setText(tabName);

        // Initializes a new BorderPane that holds all Elements.
        BorderPane borderpane = new BorderPane();

        // Holds sequence diagram.
        BorderPane required_pane = new BorderPane();

        // holds other diagrams.
        BorderPane optional_pane = new BorderPane();

        // Add sequence diagram to required pane.
        required_pane.getChildren().add(draw.getCanvas());

        // Sets the diagram on top to be the class diagram.
        optional_pane.setTop(draw.getCanvas_class());

        // Sets the diagram on the bottom to be the deployment diagram.
        optional_pane.setBottom(draw.getCanvas_deployment());

        // Sets the required pane on the left.
        borderpane.setLeft(required_pane);

        // Sets the optional pane on th eright.
        borderpane.setRight(optional_pane);

        // Set the tab view to be the pane holding all panes.
        tab.setContent(borderpane);

        // Defaults to add a sequence diagram to the view.
        addDiagram(SEQUENCE_DIAGRAM);
    }

    /**
     * Adds a diagram to the view.
     * @param match diagram to add to list of displayed ones.
     */
    public void addDiagram(String match) {

        // Check so it doesn't already exist.
        if (!viewing.contains(match))

            // Add the string to the view list.
            viewing.add(match);

        // Update the view.
        updateView();
    }

    /**
     * Removes the specified diagram if it exists in the current view.
     * @param match diagram to remove to list of displayed ones.
     */
    public void removeDiagram(String match) {

        // Check so it does exist in the list.
        if (viewing.contains(match))

            // Remove the item.
            viewing.remove(match);

        // Update the view.
        updateView();
    }

    /**
     * Re-adjusts all canvases size in according to the amount of diagrams currently being viewed.
     */
    public void updateView() {

        // Variables retrieved.
        int vSize = viewing.size();
        int width = (int)tabPane.getWidth();
        int height = (int)tabPane.getHeight();

        // Based on the number of diagrams in view their width and height are specified.
        int[] size = {width/2, height};
        final int sequence_diagram_width;

        switch (vSize) {

            case 1:
                sequence_diagram_width = width;
                break;

            case 2:
                sequence_diagram_width = width/2;
                break;

            case 3:
                sequence_diagram_width = width/2;
                size[1] = height/2;
                size[0] = width/2;
                break;

            default:
                throw new NumberFormatException("Invalid view value:" + vSize);
        }

        // UI adjustments
        Platform.runLater(() -> {

            // If class diagram is in view.
            if (!viewing.contains(CLASS_DIAGRAM)) {
                draw.getCanvas_class().setWidth(0);
                draw.getCanvas_class().setHeight(0);
            } else {
                draw.getCanvas_class().setWidth(size[0]);
                draw.getCanvas_class().setHeight(size[1]);
            }

            // If deployment diagram is in view.
            if (!viewing.contains(DEPLOYMENT_DIAGRAM)) {
                draw.getCanvas_deployment().setWidth(0);
                draw.getCanvas_deployment().setHeight(0);
            } else {
                draw.getCanvas_deployment().setWidth(size[0]);
                draw.getCanvas_deployment().setHeight(size[1]);
            }

            // If sequence diagram is in view.
            if (viewing.contains(SEQUENCE_DIAGRAM)) {
                draw.getCanvas().setWidth(sequence_diagram_width);
                draw.getCanvas().setHeight(height);
            }

            // Update their children.
            draw.redraw();
        });
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
     * Allocates and displays the ExecutionLog for the current DiagramView.
     */
    public void focus() {

        Platform.runLater(() -> {

            // Sets the execution log to display the data of this diagram view log.
            ExecutionLog.getInstance().setData(logData);

            // Update the view.
            updateView();
        });
    }

    /**
     * @param data to be added to the views data log.
     */
    public void addLogData(String data) {
        this.logData.add(data);
        if (this == getDiagramViewInView())
            ExecutionLog.getInstance().setData(this.logData);
    }
}