package visuals.handlers;

import visuals.DiagramView;
import visuals.Draw;

/**
 * Handles refreshing and animating draw objects.7
 * @version 0.1
 * @author Pontus Laestadius
 */
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
                Thread.sleep(250);
            } catch (InterruptedException e) {
                System.err.println(e.toString());
                System.out.println(e.toString());
            }

            if (DiagramView.inView(item.getName())) {
                item.update();
                item.redraw();
            }

        }
    }
}
