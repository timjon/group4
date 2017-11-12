package visuals;

/**
 * Class for the trail of the message
 * @author Isabelle Törnqvist
 * @version 1.0
 */

public class Trail {

    //Variables for coordinates of the trail smoke images
    private int xcoordinate, ycoordinate;
    //Variables for the height and width of the trail smoke images
    private double width, height;

    /**
     * Constructor
     */
    public Trail(int xcoordinate, int ycoordinate, double width, double height){
        this.xcoordinate = xcoordinate;
        this.ycoordinate = ycoordinate;
        this.width = width;
        this.height = height;
    }

    //Getter methods for coordinates, height and width
    public int getXcoordinate(){return  xcoordinate;}

    public int getYcoordinate(){return  ycoordinate;}

    public double getWidth(){return width;}

    public double getHeight(){return height;}

}
