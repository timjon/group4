package view.visuals.component;

/**
 * Class for the trail
 * @author Isabelle Törnqvist
 * @contributor Tim Jonasson
 * @version 1.1
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
     * method for resizing the trail
     * @param trailsize the new size of the trail
     * @param class_size_change the difference in window size since last update
     * @param trailOffset 
     */
    public void resize(double trailsize, double class_size_change, int trailOffset){

        //Calculates the new position
        this.setWidth(trailsize);
        this.setHeight(trailsize);
        int oldX = this.getXcoordinate();
        this.setXcoordinate((int)(oldX * class_size_change + trailOffset));
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

    /**
     * setter for the x coordinate
     * @param xcoordinate
     */

    public void setXcoordinate(int xcoordinate){
        this.xcoordinate = xcoordinate;
    }

    /**
     * setter for the y coordinate
     * @param ycoordinate
     */
    public void setYcoordinate(int ycoordinate){
        this.ycoordinate = ycoordinate;
    }

    /**
     * setter for the width
     * @param width
     */
    public void setWidth(double width){
        this.width = width;
    }

    /**
     * setter for the height
     * @param height
     */
    public void setHeight(double height){
        this.height = height;
    }
}
