package visuals;

import javafx.scene.canvas.GraphicsContext;

/**
 * Class for creating the messages to pass between classes.
 * @author Sebastian Fransson
 */
public class Message implements Renderable{
    String name;
    private Coordinates coordinates , node1, node2; // The coordinates of the nodes that the message is supposed to pass between.
   // private int offset;
    private int fromNode, toNode;

    public Message(Coordinates node1, Coordinates node2, String name/*,int offset*/, int fromNode, int toNode){ // constructor.
        this.node1 = node1;
        this.node2 = node2;
        this.name = name;
       // this.offset = offset;
        this.fromNode = fromNode;
        this.toNode = toNode;
    }

    @Override
    public Coordinates getCoordinates() { return coordinates; }

    @Override
    public String format() {
        return null;
    }

    public void Put(Coordinates node1, Coordinates node2){
        this.node1 = node1;
        this.node2 = node2;
    }

    @Override
    public void render(GraphicsContext gc){

        //fromNode Coordinates.
        int x1 = this.node1.getX();
        int y1 = this.node1.getY();
        //toNode Coordinates.
        int x2 = this.node2.getX();
        int y2 = this.node2.getY();
       // this.offset *= 1.5;
       // y1 += this.offset;
        gc.strokeLine(x1, y1, x2, y1); // Message Line.
        gc.fillText(this.name, x1+25, y1-10); // Message description.


    }

    public int getFromNode(){
        return fromNode;
    }

    public int getToNode(){
        return toNode;
    }

       public void createMessage(GraphicsContext gc){ // TODO

        //gc.strokeLine(this.x1, this.y1, this.x2, this.y1);

        //gc.fillText(this.name, x1 + 25, y1-10);

    }

}
