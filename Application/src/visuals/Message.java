package visuals;

import javafx.scene.canvas.GraphicsContext;

public class Message {

    private int x1, x2, y1, y2; // The coordinates of the nodes that the message is supposed to pass between.
    GraphicsContext gc;
    String name;
    public Message(int x1, int x2, int y1, int y2, GraphicsContext gc, String name){ // constructor.
        this.x1 = x1;
        this.x2 = x2;
        this.y1 = y1;
        this.y2 = y2;
        this.gc = gc;
        this.name = name;
    }

    public void createMessage(){ // TODO Modify for redraw and dynamic adding.

       gc.strokeLine(this.x1, this.y1, this.x2, this.y1);

        gc.fillText(this.name, x1 + 25, y1-10);

    }

}
