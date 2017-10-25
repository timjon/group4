package visuals.handlers;

import visuals.DiagramView;
import visuals.Draw;

/**
 * Handles refreshing and animating draw objects.7
 * @version 0.5
 * @author Pontus Laestadius
 */
public class Animation extends Thread {
    Thread th;

    public Animation() {}

    @Override
    public void run() {
        if (th == null) {
            th = this;
            loop();
        }
    }

    private void loop() {
        while (true) {
            try {
                Thread.sleep(200);
            } catch (InterruptedException e) {
                System.err.println(e.toString());
                System.out.println(e.toString());
            }

            Draw draw = DiagramView.getDrawInView();
            if (draw == null) continue;
            draw.update();
            draw.redraw();
        }
    }
}
