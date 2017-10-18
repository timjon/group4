package visuals;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;

import java.util.Iterator;

/**
 * A class wrapper for coordinates with extra properties that implements Renderable.
 * @author Pontus Laestadius
 */
public class Class implements Renderable {
    private Coordinates coordinates;
    private int size;
    private String name;

    private int aniindex = 0;
    private Coordinates[] anicontent;

    static Image castle = new Image("resources/castle.png");
    static Image platform = new Image("resources/platform.png");
    static Image pillar = new Image("resources/pillar.png");

    public Class(String name) {
        this.name = name;
        anicontent = new Coordinates[1];
        anicontent[0] = new Coordinates(0,0);
    }

    public void place(Coordinates coordinates, int size) {
        this.coordinates = coordinates;
        this.size = size;
    }

    public void update() {
        aniindex = aniindex == anicontent.length-1 ? 0: aniindex+1;
    }

    public String format() {
        return null;
    }

    public Coordinates getCoordinates() {
        return coordinates;
    }

    public void render(GraphicsContext gc) {
        gc.setFill(Color.TRANSPARENT);

        int x = this.coordinates.getX();
        int y = this.coordinates.getY();

        int lifeline_width = (int) (size/3.5);
        int scale = (int) (pillar.getHeight()/lifeline_width);
        for (int i = 0; i < 1000; i+=scale) {
            gc.drawImage(pillar,x +size/4 -lifeline_width/2,y + size/6 +i,lifeline_width,scale);
        }

        gc.drawImage(platform,x -size/8,y + size/2 -size/8, size/2 + size/4,size/6);
        gc.drawImage(castle, x + anicontent[aniindex].getX(), y + anicontent[aniindex].getY(), size/2, size/2);

        gc.setFill(Color.BLACK);
        int len = this.name.length();
        int pos_x = x + size/4 - len*2;
        gc.fillText(this.name, pos_x, y -15);
    }

    public String getName() {
        return name;
    }
}
