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

    //Get method for getting the x-coordinate
    public int getXcoordinate(){return  xcoordinate;}

    //Get method for getting the y-coordinate
    public int getYcoordinate(){return  ycoordinate;}

    //Get method for getting the width of the trail image
    public double getWidth(){return width;}

    //Get method for getting the height of the trail image
    public double getHeight(){return height;}

}
