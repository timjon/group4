package visuals;

import javafx.scene.canvas.GraphicsContext;

public class Message implements Renderable{
    String name;
    private Coordinates coordinates , node1, node2; // The coordinates of the nodes that the message is supposed to pass between.

    public Message(Coordinates node1, Coordinates node2, String name){ // constructor.
        this.node1 = node1;
        this.node2 = node2;
        this.name = name;
    }

    @Override
    public Coordinates getCoordinates() { return coordinates; }

    @Override
    public String format() {
        return null;
    }

    @Override
    public void render(GraphicsContext gc){
        int x1 = this.node1.getX();
        int y1 = this.node1.getY();
        int x2 = this.node2.getX();
        int y2 = this.node2.getY();

        gc.strokeLine(x1, y1, x2, y2);
        gc.fillText(this.name, x1+25, y1-10);


    }

 /*
    public void createMessage(){ // TODO Modify for redraw and dynamic adding.

       gc.strokeLine(this.x1, this.y1, this.x2, this.y1);

        gc.fillText(this.name, x1 + 25, y1-10);

    }*/

}
