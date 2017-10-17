package visuals;

import javafx.scene.canvas.GraphicsContext;

/**
 * Class for creating the messages to pass between classes.
 * @author Sebastian Fransson
 */
public class Message implements Renderable{
    String name;
    private Coordinates coordinates , node1, node2; // The coordinates of the nodes that the message is supposed to pass between.
  //  private int offset = 20; // TODO
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

    public void changeCoordinates(Coordinates node1, Coordinates node2){
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
        //this.offset += 10;
       // y1 += this.offset;
        gc.strokeLine(x1+10, y1+50, x2, y1+50); // Message Line.
        gc.fillText(this.name, x1+25, y1+40); // Message description.


    }

    public int getFromNode(){
        return fromNode;
    }

    public int getToNode(){
        return toNode;
    }

}
