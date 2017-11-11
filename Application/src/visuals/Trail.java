package visuals;

import javafx.scene.image.Image;

public class Trail {

    private int xcoor, ycoor;
    private double width, height;




    public Trail(int xcoor, int ycoor, double width, double height){
        this.xcoor = xcoor;
        this.ycoor = ycoor;
        this.width = width;
        this.height = height;


    }

    public int getXcoor(){return  xcoor;}

    public int getYcoor(){return  ycoor;}

    public double getWidth(){return width;}

    public double getHeight(){return height;}


}
