package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Observable;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.5
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<Message> messages = new ArrayList<>();

    Canvas getCanvas() {
        return canvas;
    }

    Draw(int w, int h) {
        this.canvas = new Canvas(w, h);
    }

    int getHeight() {
        return (int)canvas.getHeight();
    }

    int getWidth() {
        return (int)canvas.getHeight();
    }

    /**
     * Draws a Class on the provided canvas.
     */
    private void addClass(String name) {
        classes.add(new Class(name));
    }

    // Draws a Message.
    public void addMessage(int fromNode, int toNode, String name){ // TODO
        GraphicsContext gc = canvas.getGraphicsContext2D();
        // fromClass coordinates.
        Coordinates node1 = classes.get(fromNode).getCoordinates();
        // toClass coordinates.
        Coordinates node2 = classes.get(toNode).getCoordinates();

        Message message = new Message(node1, node2, name);
        messages.add(message);
    }

    void render() {
        init();
        renderClass();
        renderMessage();
        renderContainer();
    }

    // Always renders with a new specific resolution.
    void resize(int w, int h) {
        if (w == getWidth() && h == getHeight())
            return;
        canvas.setHeight(h);
        canvas.setWidth(w);
        render();
    }

    void init() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        gc.clearRect(0,0,getWidth(), getHeight());
        gc.setFill(Color.GREY);
        gc.strokeRoundRect(0,-1,getWidth(),getHeight()+1, 0,0);
        gc.setFill(Color.BLACK);
    }

    void renderContainer() {
        GraphicsContext gc = canvas.getGraphicsContext2D();
        for (Renderable r: classes)
            r.render(gc);
        for (Renderable r: messages)
            r.render(gc);
    }

    void renderMessage() {
        //What? confusion is real...
        //---------------------------
        // Ok, what you do is do all computing stuff here.
        // Then let magic do the rest.
        // see renderClass for reference.
    }

    void renderClass() {
        if (classes.size() == 0) return;
        // The amount of space each class can use.
        int x_offset = getWidth()/100;
        int space = (getWidth()-x_offset*3)/this.classes.size();
        int size = space/2;
        for(int i = 0; i < classes.size(); i++) {
            int x = size+ (i*space) +x_offset;
            int y = (i % 2 == 0 ? 70:73);
            classes.get(i).place(new Coordinates(x,y), size);
        }
    }

    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------
    //----------------------------TEST CODE BELOW------------------------------------
    //-------------------------------------------------------------------------------
    //-------------------------------------------------------------------------------

    void example_diagram() {
        // Classes
        this.addClass("test1");
        this.addClass("test2");
        this.addClass("test3");
        this.addClass("test4");
        this.addClass("test5 long name");
        this.addClass("test6");
        this.addClass("test7");
        this.addClass("test8");
        this.addClass("test9");
        // Messages
        this.addMessage(0, 1, "Message 1");
        this.addMessage(3, 4, "Message 2");
    }

    public static void temp_generate_diagram() {
        // Init's a draw object that handles graphical elements
        Draw draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw.example_diagram(); // Only used to display an example.
        DiagramView dv = new DiagramView(draw, "diagram name");
        tabPane.getTabs().add(dv.getTab());
        // Renders and displays the classes
        draw.render();
    }
}
