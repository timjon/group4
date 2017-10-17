package visuals;

import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;

public class RenderHandler<T extends Renderable> extends Thread {
    ArrayList<T> list;
    GraphicsContext gc;

    RenderHandler(GraphicsContext gc, ArrayList<T> list) {
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
