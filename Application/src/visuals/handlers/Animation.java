package visuals.handlers;

import visuals.DiagramView;
import visuals.Draw;

public class Animation extends Thread {
    Draw item;

    public Animation(Draw item) {
        this.item = item;
    }

    @Override
    public void run() {
         loop();
    }

    private void loop() {
        while (!DiagramView.list.isEmpty()) {
            try {
                Thread.sleep(1000/60);
            } catch (InterruptedException e) {
                System.err.println(e.toString());
            }
            item.update();
            item.redraw();
        }
    }
}
