package visuals;

/**
 * Class for the trail
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

    /**
     * Getter method for getting the x-coordinate for the trail image
     * @return xcoordinate
     */
    public int getXcoordinate(){return  xcoordinate;}

    /**
     * Getter method for getting the y-coordinate for the trail image
     * @return ycoordinate
     */
    public int getYcoordinate(){return  ycoordinate;}

    /**
     * Getter method for getting the width of the trail image
     * @return width
     */
    public double getWidth(){return width;}

    /**
     * Getter method for getting the height of the trail image
     * @return height
     */
    public double getHeight(){return height;}

}
