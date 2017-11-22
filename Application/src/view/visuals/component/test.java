package view.visuals.component;

import javafx.scene.image.Image;

public class test {

    //Manual testing of bridges
    private static Image island = new Image("resources/island.png");
    private static Image bridge = new Image("resources/bridgeSprite.png");
    private static Image arrow = new Image("resources/arrow1.png");


    // after the classes of the classDiagram are visualised in their own canvas, implement the following

    // example island1, island2, island2 inherits from island1

    // int xDistance = island2.getX - island1.getX;
    // int yDistance = island2.getY - island1.getY;
    // if there is a difference in the vertical distance
    // example : yDistance = 200
    // for ( i = 0; i< yDistance/bridgeSpriteDimension) { // case where there is a vertical difference
    // draw a bridge at current position. move towards goal on X axis by bridgeSpriteDimension amount of pixels
    // then draw again. then move towards goal on Y axis by the same amount and draw again
    // the for loop completes when you have moved enough to the goal on both X and Y axes
    // this is ensured by finding the ratio between X displacement and Y displacement.


    // stuck on : finding out how to reach a coordinate tuple to be accessed
    //so i reference the positions of these objects and determine how to draw my "bridge"

}
