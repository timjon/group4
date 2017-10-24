package visuals.handlers;

import javafx.scene.canvas.GraphicsContext;
import visuals.Renderable;

import java.util.ArrayList;

public class Render<T extends Renderable> extends Thread {
    ArrayList<T> list;
    GraphicsContext gc;

    public Render(GraphicsContext gc, ArrayList<T> list) {
        this.gc = gc;
        this.list = list;
    }

    @Override
    public void run() {
        for (Renderable r: list) {
            r.render(gc);
        }
    }


}
