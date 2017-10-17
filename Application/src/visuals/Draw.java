package visuals;

import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Observable;

import static visuals.DiagramView.tabPane;

/**
 * @version 0.55
 * @author Pontus Laestadius, Sebastian Fransson
 */

public class Draw {

    private Canvas canvas;
    private ArrayList<Class> classes = new ArrayList<>();
    private ArrayList<Message> messages = new ArrayList<>();
   // private static int offset;

    Canvas getCanvas() {
        return canvas;
    }

    Draw(int w, int h) {
        canvas = new Canvas(w, h);
    }

    int getHeight() {
        return (int)canvas.getHeight();
    }

    int getWidth() {
        return (int)canvas.getWidth();
    }

    /**
     * Draws a Class on the provided canvas.
     */
    private void addClass(String name) {
        classes.add(new Class(name));
    }

    // Draws a Message.
    public void addMessage(int fromNode, int toNode, String name){ // TODO
        this.messages.add(new Message(classes.get(fromNode).getCoordinates(), classes.get(toNode).getCoordinates(), name, fromNode, toNode));
       /*
         GraphicsContext gc = this.canvas.getGraphicsContext2D();
        // fromClass coordinates.
        Coordinates node1 = classes.get(fromNode).getCoordinates();
        // toClass coordinates.
        Coordinates node2 = items.get(toNode).getCoordinates();
        offset += 1;
        Message message = new Message(node1, node2, name, offset);
        message.render(gc);
        */
    }

    void render() {
        init();
        renderClass();
        renderMessage();
        renderContainer();
    }

    // Always renders with a new specific resolution.
    void resize(double w, double h) {
        if (w == getWidth() && h == getHeight())
            return;
        System.out.println("w:" + w + " h:" + h);
        canvas.setWidth(w);
        canvas.setHeight(h);
        System.out.println("render: " + getWidth() + "x" + getHeight());
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
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        if(this.messages.size() > 0) {
            for (int i = 0; i < messages.size(); i++) {
                Coordinates node1 = classes.get(messages.get(i).getFromNode()).getCoordinates();
                Coordinates node2 = classes.get(messages.get(i).getToNode()).getCoordinates();

                messages.get(i).Put(node1, node2);
            }
        }

        //What? confusion is real...
        //---------------------------
        // Ok, what you do is do all computing stuff here.
        // Then let magic do the rest.
        // see renderClass for reference.
    }

    void renderClass() {
        if (classes.size() == 0) return; // There are no items to render
        int space = (getWidth())/this.classes.size(); // The amount of space each class can use.
        int size = space/2; // The size of the objects is half of it's given space.
        for(int i = 0; i < classes.size(); i++) {
            int x = size+ (i*space);
            int y = (i % 2 == 0 ? 70:71);
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
        for (int i = 0; i < 10; i++)
            this.addClass("test" + i);
        // Messages
        this.addMessage(0, 1, "Message 1");
        this.addMessage(3, 4, "Message 2");
    }

    public static void temp_generate_diagram() { // Init's a draw object that handles graphical elements
        Draw draw = new Draw((int)tabPane.getWidth(), (int)tabPane.getHeight());
        draw.example_diagram(); // Only used to display an example.
        DiagramView dv = new DiagramView(draw, "diagram name");
        tabPane.getTabs().add(dv.getTab());
        draw.render(); // Renders and displays items
    }
}
